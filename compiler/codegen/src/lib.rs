use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{AnyValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::AddressSpace;
use parse::ast;
use std::collections::HashMap;
use std::error::Error;
use typeck::mir;
use typeck::mir::Typed;
use typeck::scope::{Def, DefId};

pub fn codegen(fun: mir::Fun) {
    let ctx = Context::create();
    let mut codegen = Codegen::new(&ctx);
    codegen.build_fun(fun);
    codegen.module.print_to_stderr();
}

#[derive(Debug)]
struct Function<'ctx> {
    fn_value: FunctionValue<'ctx>,
    builder: Builder<'ctx>,
    // ret_type: mir::Type,
    // ret_value_ptr: Option<PointerValue<'static>>,
    var_map: HashMap<DefId, Variable<'ctx>>,
    // arg_names: Vec<Arc<String>>,
    // vars_by_name: HashMap<String, Variable>,
}

impl<'ctx> Function<'ctx> {
    fn new(fn_value: FunctionValue<'ctx>, builder: Builder<'ctx>) -> Self {
        Self {
            fn_value,
            builder,
            var_map: HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Variable<'ctx> {
    pub name: String,
    pub ty: mir::Type,
    pub is_arg: bool,
    pub ptr: PointerValue<'ctx>,
}

impl<'ctx> Variable<'ctx> {
    fn new(name: String, ty: mir::Type, is_arg: bool, ptr: PointerValue<'ctx>) -> Self {
        Self {
            name,
            ty,
            is_arg,
            ptr,
        }
    }
}

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    function: Option<Function<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        Self {
            context,
            module: context.create_module("fenec"),
            function: None,
        }
    }

    fn build_function(
        &mut self,
        name: &str,
        ret_ty: mir::Type,
        arg_tys: &[mir::Type],
    ) -> Function<'ctx> {
        let param_types: Vec<BasicTypeEnum<'ctx>> = arg_tys
            .iter()
            .map(|arg_ty| self.llvm_basic_ty(arg_ty))
            .collect();
        let fn_type = match ret_ty {
            mir::Type::Int(int_ty) => match int_ty {
                mir::IntTy::I8 => self.context.i8_type(),
                mir::IntTy::I16 => self.context.i16_type(),
                mir::IntTy::I32 => self.context.i32_type(),
                mir::IntTy::I64 => self.context.i64_type(),
            }
            .fn_type(&param_types, false),
            mir::Type::Float(float_ty) => match float_ty {
                mir::FloatTy::F32 => self.context.f32_type(),
                mir::FloatTy::F64 => self.context.f64_type(),
            }
            .fn_type(&param_types, false),
            mir::Type::Bool => self.context.bool_type().fn_type(&param_types, false),
            mir::Type::Void => self.context.void_type().fn_type(&param_types, false),
            _ => unimplemented!(),
        };
        Function::new(
            self.module.add_function(name, fn_type, None),
            self.context.create_builder(),
        )
    }

    fn function(&self) -> &Function<'ctx> {
        self.function.as_ref().unwrap()
    }

    fn function_mut(&mut self) -> &mut Function<'ctx> {
        self.function.as_mut().unwrap()
    }

    fn builder(&self) -> &Builder<'ctx> {
        &self.function().builder
    }

    fn append_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        self.context
            .append_basic_block(self.function().fn_value, name)
    }

    fn set_basic_block(&self, basic_block: BasicBlock<'ctx>) {
        self.function().builder.position_at_end(basic_block);
    }

    fn build_fun(&mut self, fun: mir::Fun) {
        self.function = Some(self.build_function(&fun.name.raw, fun.ret_ty, &fun.def.arg_tys));
        // append and set basic block
        let start_bb = self.append_basic_block("start");
        self.set_basic_block(start_bb);
        // args
        self.build_fun_args(&fun);
        // block
        for stmt in fun.block.stmts.into_iter() {
            self.build_stmt(stmt);
        }
    }

    fn build_fun_args(&mut self, fun: &mir::Fun) {
        for (idx, ty) in fun.def.arg_tys.iter().enumerate() {
            let name = fun.args[idx].raw.clone();
            let param = self.function().fn_value.get_nth_param(idx as u32).unwrap();
            let ptr = self.builder().build_alloca(param.get_type(), &name);
            self.builder().build_store(ptr, param);
            self.function_mut()
                .var_map
                .insert(fun.args[idx].def.id(), Variable::new(name, *ty, true, ptr));
        }
    }

    fn build_stmt(&mut self, stmt: mir::Stmt) {
        match stmt {
            mir::Stmt::VarDecl(var_decl) => {
                let name = var_decl.name.raw;
                let val = self.build_expr(*var_decl.init);
                let ptr = self
                    .builder()
                    .build_alloca(self.llvm_basic_ty(&var_decl.def.ty), &name);
                self.builder().build_store(ptr, val);
                self.function_mut().var_map.insert(
                    var_decl.def.id,
                    Variable::new(name, var_decl.def.ty, false, ptr),
                );
            }
            mir::Stmt::Ret(ret) => match ret.expr {
                Some(expr) => {
                    let val = self.build_expr(expr);
                    self.builder().build_return(Some(match val {
                        BasicValueEnum::ArrayValue(ref value) => value,
                        BasicValueEnum::IntValue(ref value) => value,
                        BasicValueEnum::FloatValue(ref value) => value,
                        BasicValueEnum::PointerValue(ref value) => value,
                        BasicValueEnum::StructValue(ref value) => value,
                        BasicValueEnum::VectorValue(ref value) => value,
                    }));
                }
                None => {
                    self.builder().build_return(None);
                }
            },
            mir::Stmt::Expr(expr) => {
                self.build_expr(expr);
            }
        }
    }

    fn build_expr(&mut self, expr: mir::Expr) -> BasicValueEnum<'ctx> {
        match expr {
            mir::Expr::Lit(lit) => {
                let ty = lit.get_type();
                let basic_ty = self.llvm_basic_ty(&ty);
                match lit.kind {
                    ast::LitKind::Int(v) => basic_ty.into_int_type().const_int(v, true).into(),
                    ast::LitKind::Float(v) => basic_ty.into_float_type().const_float(v).into(),
                    ast::LitKind::Bool(v) => basic_ty
                        .into_int_type()
                        .const_int(if v { 1 } else { 0 }, true)
                        .into(),
                    ast::LitKind::String(v) => {
                        unimplemented!()
                    }
                }
            }
            mir::Expr::Ident(ident) => self.builder().build_load(
                self.function().var_map.get(&ident.def.id()).unwrap().ptr,
                "",
            ),
            mir::Expr::Binary(binary) => unimplemented!(),
            mir::Expr::Unary(unary) => unimplemented!(),
        }
    }

    fn llvm_basic_ty(&self, ty: &mir::Type) -> BasicTypeEnum<'ctx> {
        match ty {
            mir::Type::Int(int_ty) => match int_ty {
                mir::IntTy::I8 => self.context.i8_type(),
                mir::IntTy::I16 => self.context.i16_type(),
                mir::IntTy::I32 => self.context.i32_type(),
                mir::IntTy::I64 => self.context.i64_type(),
            }
            .into(),
            mir::Type::Float(float_ty) => match float_ty {
                mir::FloatTy::F32 => self.context.f32_type(),
                mir::FloatTy::F64 => self.context.f64_type(),
            }
            .into(),
            mir::Type::Bool => self.context.bool_type().into(),
            _ => unimplemented!(),
        }
    }
}
