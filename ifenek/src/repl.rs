use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{AnyValueEnum, BasicValueEnum, FunctionValue, GenericValue, PointerValue};
use parse::ast;
use std::collections::HashMap;
use std::error::Error;
use typeck::mir;
use typeck::mir::Typed;
use typeck::scope::Def;
use typeck::typeck::TypeCk;

pub struct Repl<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    engine: ExecutionEngine<'ctx>,
    typeck: TypeCk,

    value_map: HashMap<usize, BasicValueEnum<'ctx>>,
}

impl<'ctx> Repl<'ctx> {
    pub fn create(context: &'ctx Context, module: &str) -> Result<Self, Box<dyn Error>> {
        let module = context.create_module(module);
        let builder = context.create_builder();
        let engine = module.create_interpreter_execution_engine()?;
        let typeck = TypeCk::new();
        Ok(Self {
            context,
            module,
            builder,
            engine,
            typeck,
            value_map: HashMap::new(),
        })
    }

    pub fn eval(&mut self, input: &str) -> Result<(), Box<dyn Error>> {
        let stmt = parse::parse(&input)?;
        let mir_stmt = self.typeck.typecheck_stmt(&stmt)?;
        printer::print(&mir_stmt)?;

        let val = match mir_stmt {
            mir::Stmt::VarDecl(var_decl) => self.eval_var_decl(var_decl)?,
            mir::Stmt::Expr(expr) => self.eval_expr(&expr)?,
        };
        self.print_basic_val(val);

        Ok(())
    }

    fn print_basic_val(&self, val: BasicValueEnum<'ctx>) {
        match val.get_type() {
            BasicTypeEnum::IntType(_) => val.into_int_value().print_to_stderr(),
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
                ast::LitKind::Int(v) => {
                    let llvm_ty = match lit.get_type().into_int_ty() {
                        mir::IntTy::I8 => self.context.i8_type(),
                        mir::IntTy::I16 => self.context.i16_type(),
                        mir::IntTy::I32 => self.context.i32_type(),
                        mir::IntTy::I64 => self.context.i64_type(),
                    };
                    Ok(llvm_ty.const_int(v, false).into())
                }
                _ => unimplemented!(),
            },
            mir::Expr::Ident(ident) => Ok(*self.value_map.get(&ident.def.id()).unwrap()),
            mir::Expr::Binary(binary) => {
                let lhs = self.eval_expr(&binary.lhs)?;
                let rhs = self.eval_expr(&binary.rhs)?;
                let res = match binary.op.symbol.as_str() {
                    "+" => {
                        self.builder
                            .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "")
                    }
                    "-" => {
                        self.builder
                            .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "")
                    }
                    _ => unimplemented!(),
                };

                Ok(res.into())
            }
            _ => {
                unimplemented!()
            }
        }
    }
}
