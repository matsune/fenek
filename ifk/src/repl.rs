use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{AnyValueEnum, BasicValueEnum, FunctionValue, IntValue};
use parse::ast;
use std::collections::HashMap;
use std::error::Error;
use typeck::mir;
use typeck::mir::Typed;
use typeck::scope::Def;
use typeck::typeck::TypeCk;

const MODULE_NAME: &'static str = "ifk_repl";

pub struct Repl<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    typeck: TypeCk,

    value_map: HashMap<usize, BasicValueEnum<'ctx>>,
}

impl<'ctx> Repl<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module(MODULE_NAME);
        let builder = context.create_builder();
        // let engine = module.create_interpreter_execution_engine()?;
        let typeck = TypeCk::new();
        Self {
            context,
            module,
            builder,
            typeck,
            value_map: HashMap::new(),
        }
    }

    pub fn eval(&mut self, input: &str) -> Result<(), Box<dyn Error>> {
        self.module = self.context.create_module(MODULE_NAME);
        self.builder = self.context.create_builder();

        let stmt = parse::parse(&input)?;
        let mir_stmt = self.typeck.typecheck_stmt(&stmt)?;
        printer::print(&mir_stmt)?;

        match mir_stmt {
            mir::Stmt::VarDecl(var_decl) => {
                let val = self.eval_var_decl(var_decl)?;
                self.print_basic_val(val);
            }
            mir::Stmt::Expr(expr) => {
                let ret_type = self.get_llvm_basic_ty(&expr.get_type());
                let fn_type = ret_type.fn_type(&[], false);
                let anon_main_fn = self.module.add_function("anon_main", fn_type, None);
                self.builder
                    .position_at_end(self.context.append_basic_block(anon_main_fn, "entry"));
                let v = self.eval_expr(&expr)?;
                match expr.get_type() {
                    mir::Type::Int(_) | mir::Type::Bool => {
                        self.builder.build_return(Some(&v.into_int_value()))
                    }
                    mir::Type::Float(_) => self.builder.build_return(Some(&v.into_float_value())),
                    _ => unimplemented!(),
                };
                let engine = self.module.create_interpreter_execution_engine()?;
                let val = unsafe { engine.run_function(anon_main_fn, &[]) };
                match expr.get_type() {
                    mir::Type::Int(_) => println!("{}", val.as_int(true)),
                    mir::Type::Bool => {
                        println!("{}", val.as_int(true) != 0)
                    }
                    mir::Type::Float(_) => {
                        println!(
                            "{}",
                            val.as_float(
                                &self
                                    .get_llvm_basic_ty(&expr.get_type())
                                    .as_basic_type_enum()
                                    .into_float_type()
                            )
                        );
                    }
                    mir::Type::Float(mir::FloatTy::F32) => {
                        println!("{}", val.as_float(&self.context.f32_type()))
                    }
                    _ => unimplemented!(),
                };
            }
        };

        Ok(())
    }

    fn get_llvm_basic_ty(&self, ty: &mir::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match ty {
            mir::Type::Int(int_ty) => match int_ty {
                mir::IntTy::I8 => Box::new(self.context.i8_type()),
                mir::IntTy::I16 => Box::new(self.context.i16_type()),
                mir::IntTy::I32 => Box::new(self.context.i32_type()),
                mir::IntTy::I64 => Box::new(self.context.i64_type()),
            },
            mir::Type::Float(float_ty) => match float_ty {
                mir::FloatTy::F32 => Box::new(self.context.f32_type()),
                mir::FloatTy::F64 => Box::new(self.context.f64_type()),
            },
            mir::Type::Bool => Box::new(self.context.bool_type()),
            _ => unimplemented!(),
        }
    }

    fn print_basic_val(&self, val: BasicValueEnum<'ctx>) {
        match val.get_type() {
            BasicTypeEnum::IntType(_) => val.into_int_value().print_to_stderr(),
            BasicTypeEnum::FloatType(_) => val.into_float_value().print_to_stderr(),
            _ => unimplemented!(),
        }
        println!();
    }

    fn eval_var_decl(
        &mut self,
        var_decl: mir::VarDecl,
    ) -> Result<BasicValueEnum<'ctx>, Box<dyn Error>> {
        let expr_val = self.eval_expr(&var_decl.init)?;
        self.value_map.insert(var_decl.def.id(), expr_val);
        Ok(expr_val)
    }

    fn eval_expr(&mut self, expr: &mir::Expr) -> Result<BasicValueEnum<'ctx>, Box<dyn Error>> {
        match expr {
            mir::Expr::Lit(lit) => match lit.kind {
                ast::LitKind::Int(v) => Ok(self
                    .get_llvm_basic_ty(&lit.get_type())
                    .as_basic_type_enum()
                    .into_int_type()
                    .const_int(v, false)
                    .into()),
                ast::LitKind::Float(v) => Ok(self
                    .get_llvm_basic_ty(&lit.get_type())
                    .as_basic_type_enum()
                    .into_float_type()
                    .const_float(v)
                    .into()),
                ast::LitKind::Bool(v) => Ok(self
                    .context
                    .bool_type()
                    .const_int(if v { 1 } else { 0 }, false)
                    .into()),
                _ => unimplemented!(),
            },
            mir::Expr::Ident(ident) => Ok(*self.value_map.get(&ident.def.id()).unwrap()),
            mir::Expr::Binary(binary) => {
                let ty = binary.get_type();
                let lhs = self.eval_expr(&binary.lhs)?;
                let rhs = self.eval_expr(&binary.rhs)?;
                let res = match ty {
                    mir::Type::Int(int_ty) => match int_ty {
                        mir::IntTy::I64 => match binary.op.symbol.as_str() {
                            "+" => self.builder.build_int_add(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                "",
                            ),
                            "-" => self.builder.build_int_sub(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                "",
                            ),
                            "*" => self.builder.build_int_mul(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                "",
                            ),
                            "/" => self.builder.build_int_signed_div(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                "",
                            ),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    _ => unimplemented!(),
                };
                Ok(res.into())
            }
            mir::Expr::Unary(unary) => {
                unimplemented!()
            }
        }
    }

    //     fn build_int_add(
    //         &self,
    //         lhs: IntValue,
    //         rhs: IntValue,
    //     ) -> Result<IntValue<'ctx>, Box<dyn Error>> {
    //         unsafe {
    //             let int_add_fn: JitFunction<unsafe extern "C" fn(i64, i64) -> i64> = self
    //                 .engine
    //                 .get_function(format!("int_add_{:?}", lhs.get_type()).as_str())
    //                 .unwrap();
    //         }
    //         unimplemented!();
    //     }

    //     fn build_int_sub(
    //         &self,
    //         lhs: IntValue,
    //         rhs: IntValue,
    //     ) -> Result<IntValue<'ctx>, Box<dyn Error>> {
    //         unimplemented!()
    //     }
}
