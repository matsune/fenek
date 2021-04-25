use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntMathValue, IntValue, PointerValue};
use inkwell::AddressSpace;
use parse::ast;
use std::collections::HashMap;
use typeck::hir;
use typeck::hir::Typed;
use typeck::scope::DefId;
use typeck::ty;

fn llvm_intrinsic_type_name(ty: BasicTypeEnum) -> String {
    match ty {
        BasicTypeEnum::ArrayType(_) => unimplemented!(),
        BasicTypeEnum::IntType(ty) => format!("i{}", ty.get_bit_width()),
        BasicTypeEnum::FloatType(_) => unimplemented!(),
        BasicTypeEnum::PointerType(_) => unimplemented!(),
        BasicTypeEnum::StructType(_) => unimplemented!(),
        BasicTypeEnum::VectorType(ty) => format!(
            "v{}{}",
            ty.get_size(),
            llvm_intrinsic_type_name(ty.get_element_type())
        ),
    }
}

#[derive(Debug)]
struct Function<'ctx> {
    fn_value: FunctionValue<'ctx>,
    builder: Builder<'ctx>,
    var_map: HashMap<DefId, Variable<'ctx>>,
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
    pub ty: ty::Type,
    pub is_arg: bool,
    pub ptr: PointerValue<'ctx>,
}

impl<'ctx> Variable<'ctx> {
    fn new(name: String, ty: ty::Type, is_arg: bool, ptr: PointerValue<'ctx>) -> Self {
        Self {
            name,
            ty,
            is_arg,
            ptr,
        }
    }
}

// pub trait PhiMergeable<'ctx>: Sized {
//     fn merge(
//         self,
//         other: Self,
//         self_bb: BasicBlock<'ctx>,
//         other_bb: BasicBlock<'ctx>,
//         codegen: &mut Codegen<'ctx>,
//     ) -> Self;
// }

// impl<'ctx> PhiMergeable<'ctx> for () {
//     fn merge(
//         self,
//         other: Self,
//         self_bb: BasicBlock<'ctx>,
//         other_bb: BasicBlock<'ctx>,
//         codegen: &mut Codegen<'ctx>,
//     ) -> Self {
//     }
// }

// impl<'ctx> PhiMergeable<'ctx> for BasicValueEnum<'ctx> {
//     fn merge(
//         self,
//         other: Self,
//         self_bb: BasicBlock<'ctx>,
//         other_bb: BasicBlock<'ctx>,
//         codegen: &mut Codegen<'ctx>,
//     ) -> Self {
//         let phi = codegen
//             .builder()
//             .build_phi(self.get_type(), "mergeValuesEndIf");
//         phi.add_incoming(&[(&self, self_bb), (&other, other_bb)]);
//         phi.as_basic_value()
//     }
// }

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

    pub fn output_to_file<P: AsRef<std::path::Path>>(&self, path: P) -> Result<(), LLVMString> {
        self.module.print_to_file(path)
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

    fn current_block(&mut self) -> BasicBlock<'ctx> {
        self.builder().get_insert_block().unwrap()
    }

    fn needs_terminator(&mut self) -> bool {
        self.builder()
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
    }

    pub fn get_llvm_intrinisic(
        &mut self,
        name: &str,
        fn_type: FunctionType<'ctx>,
    ) -> FunctionValue<'ctx> {
        match self.module.get_function(name) {
            Some(fn_value) => {
                if fn_value.get_type() == fn_type {
                    fn_value
                } else {
                    panic!("Requested multiple LLVM intrinsics with same name but different type signatures")
                }
            }
            None => self.module.add_function(name, fn_type, None),
        }
    }

    fn llvm_basic_ty(&self, ty: &ty::Type) -> BasicTypeEnum<'ctx> {
        match ty {
            ty::Type::Int(int_ty) => match int_ty {
                ty::IntKind::I8 => self.context.i8_type(),
                ty::IntKind::I16 => self.context.i16_type(),
                ty::IntKind::I32 => self.context.i32_type(),
                ty::IntKind::I64 => self.context.i64_type(),
            }
            .into(),
            ty::Type::Float(float_ty) => match float_ty {
                ty::FloatKind::F32 => self.context.f32_type(),
                ty::FloatKind::F64 => self.context.f64_type(),
            }
            .into(),
            ty::Type::Bool => self.context.bool_type().into(),
            _ => unimplemented!(),
        }
    }

    fn build_function(
        &mut self,
        name: &str,
        ret_ty: &ty::Type,
        arg_tys: &[ty::Type],
    ) -> Function<'ctx> {
        let param_types: Vec<BasicTypeEnum<'ctx>> = arg_tys
            .iter()
            .map(|arg_ty| self.llvm_basic_ty(arg_ty))
            .collect();
        let fn_type = match ret_ty {
            ty::Type::Int(int_ty) => match int_ty {
                ty::IntKind::I8 => self.context.i8_type(),
                ty::IntKind::I16 => self.context.i16_type(),
                ty::IntKind::I32 => self.context.i32_type(),
                ty::IntKind::I64 => self.context.i64_type(),
            }
            .fn_type(&param_types, false),
            ty::Type::Float(float_ty) => match float_ty {
                ty::FloatKind::F32 => self.context.f32_type(),
                ty::FloatKind::F64 => self.context.f64_type(),
            }
            .fn_type(&param_types, false),
            ty::Type::Bool => self.context.bool_type().fn_type(&param_types, false),
            ty::Type::Void => self.context.void_type().fn_type(&param_types, false),
            _ => unimplemented!(),
        };
        Function::new(
            self.module.add_function(name, fn_type, None),
            self.context.create_builder(),
        )
    }

    pub fn build_fun(&mut self, fun: &hir::Fun) {
        self.function = Some(self.build_function(&fun.name.raw, &fun.ret_ty, &fun.def.arg_tys));
        // append and set basic block
        let start_bb = self.append_basic_block("start");
        self.set_basic_block(start_bb);
        // args
        self.build_fun_args(&fun);
        // block
        for stmt in fun.block.stmts.iter() {
            self.build_stmt(stmt);
        }
    }

    fn build_fun_args(&mut self, fun: &hir::Fun) {
        for (idx, ty) in fun.def.arg_tys.iter().enumerate() {
            let name = fun.args[idx].raw.clone();
            let param = self.function().fn_value.get_nth_param(idx as u32).unwrap();
            let ptr = self.builder().build_alloca(param.get_type(), &name);
            self.builder().build_store(ptr, param);
            self.function_mut().var_map.insert(
                fun.args[idx].def.id(),
                Variable::new(name, ty.clone(), true, ptr),
            );
        }
    }

    fn build_stmt(&mut self, stmt: &hir::Stmt) {
        match stmt {
            hir::Stmt::VarDecl(var_decl) => {
                let name = &var_decl.name.raw;
                let val = self.build_expr(&var_decl.init);
                let ptr = self
                    .builder()
                    .build_alloca(self.llvm_basic_ty(&var_decl.def.ty), &name);
                self.builder().build_store(ptr, val);
                self.function_mut().var_map.insert(
                    var_decl.def.id,
                    Variable::new(name.clone(), var_decl.def.ty.clone(), false, ptr),
                );
            }
            hir::Stmt::Ret(ret) => match &ret.expr {
                Some(expr) => {
                    let val = self.build_expr(&expr);
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
            hir::Stmt::Expr(expr) => {
                self.build_expr(&expr);
            }
        }
    }

    fn build_expr(&mut self, expr: &hir::Expr) -> BasicValueEnum<'ctx> {
        match expr {
            hir::Expr::Lit(lit) => {
                let basic_ty = self.llvm_basic_ty(&lit.ty);
                match &lit.kind {
                    hir::LitKind::I8(v) => {
                        basic_ty.into_int_type().const_int(*v as u64, true).into()
                    }
                    hir::LitKind::I16(v) => {
                        basic_ty.into_int_type().const_int(*v as u64, true).into()
                    }
                    hir::LitKind::I32(v) => {
                        basic_ty.into_int_type().const_int(*v as u64, true).into()
                    }
                    hir::LitKind::I64(v) => {
                        basic_ty.into_int_type().const_int(*v as u64, true).into()
                    }
                    hir::LitKind::F32(v) => {
                        basic_ty.into_float_type().const_float(*v as f64).into()
                    }
                    hir::LitKind::F64(v) => basic_ty.into_float_type().const_float(*v).into(),
                    hir::LitKind::Bool(v) => basic_ty
                        .into_int_type()
                        .const_int(if *v { 1 } else { 0 }, true)
                        .into(),
                    // ast::LitKind::String(v) => {
                    //     unimplemented!()
                    // }
                }
            }
            hir::Expr::Ident(ident) => self.builder().build_load(
                self.function().var_map.get(&ident.def.id()).unwrap().ptr,
                &ident.raw,
            ),
            hir::Expr::Binary(binary) => {
                let lhs = self.build_expr(&binary.lhs);
                let rhs = self.build_expr(&binary.rhs);
                match binary.ty {
                    ty::Type::Int(_) => {
                        let lhs = lhs.into_int_value();
                        let rhs = rhs.into_int_value();
                        match binary.op.as_str() {
                            "+" => self.build_checked_int_arithmetic(lhs, rhs, "sadd"),
                            "-" => self.build_checked_int_arithmetic(lhs, rhs, "ssub"),
                            "*" => self.build_checked_int_arithmetic(lhs, rhs, "smul"),
                            "/" => self.builder().build_int_signed_div(lhs, rhs, "sdiv").into(),
                            _ => unimplemented!(),
                        }
                    }
                    ty::Type::Float(_) => {
                        let lhs = lhs.into_float_value();
                        let rhs = rhs.into_float_value();
                        match binary.op.as_str() {
                            "+" => self.builder().build_float_add(lhs, rhs, "fadd").into(),
                            "-" => self.builder().build_float_sub(lhs, rhs, "fsub").into(),
                            "*" => self.builder().build_float_mul(lhs, rhs, "fmul").into(),
                            "/" => self.builder().build_float_div(lhs, rhs, "fdiv").into(),
                            _ => unimplemented!(),
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            hir::Expr::Unary(unary) => match unary.op {
                ast::UnaryOpKind::Add => self.build_expr(&unary.expr),
                ast::UnaryOpKind::Sub => {
                    let expr = self.build_expr(&unary.expr);
                    match unary.get_type() {
                        ty::Type::Int(_) => self
                            .builder()
                            .build_int_neg(expr.into_int_value(), "neg")
                            .into(),
                        ty::Type::Float(_) => self
                            .builder()
                            .build_float_neg(expr.into_float_value(), "neg")
                            .into(),
                        _ => unimplemented!(),
                    }
                }
                ast::UnaryOpKind::Not => {
                    let expr = self.build_expr(&unary.expr);
                    match unary.get_type() {
                        ty::Type::Bool => self
                            .builder()
                            .build_int_neg(expr.into_int_value(), "neg")
                            .into(),
                        _ => unreachable!(),
                    }
                }
            },
        }
    }

    pub fn build_checked_int_arithmetic<T: IntMathValue<'ctx>>(
        &mut self,
        lhs: T,
        rhs: T,
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        let arg_type = lhs.as_basic_value_enum().get_type();

        let intrinsic_name = format!(
            "llvm.{}.with.overflow.{}",
            name,
            llvm_intrinsic_type_name(arg_type)
        );
        let bool_type;
        if arg_type.is_vector_type() {
            bool_type = self
                .context
                .bool_type()
                .vec_type(arg_type.into_vector_type().get_size())
                .into();
        } else {
            bool_type = self.context.bool_type().into();
        }
        let intrinsic_return_type = self.context.struct_type(&[arg_type, bool_type], false);
        let intrinsic_fn_type = intrinsic_return_type.fn_type(&[arg_type; 2], false);
        let intrinsic_fn = self.get_llvm_intrinisic(&intrinsic_name, intrinsic_fn_type);
        let intrinsic_args = &[lhs.as_basic_value_enum(), rhs.as_basic_value_enum()];

        let return_value = self
            .builder()
            .build_call(intrinsic_fn, intrinsic_args, "tmp_checked_result")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_struct_value();

        let result_value = self
            .builder()
            .build_extract_value(return_value, 0, "tmp_result")
            .unwrap();
        let is_overflow_vec = self
            .builder()
            .build_extract_value(return_value, 1, "tmp_overflow")
            .unwrap();
        let is_overflow = self.build_reduce("or", is_overflow_vec);

        self.build_conditional(
            is_overflow,
            // Return an error if there is overflow.
            |self_| {
                self_.intrinsic_puts(&format!("attempt to {} with overflow", name));
                self_.intrinsic_exit(1);
            },
            // Otherwise proceed.
            |_| (),
        );

        result_value
    }

    fn intrinsic_puts(&mut self, msg: &str) {
        let puts = self.get_llvm_intrinisic(
            "puts",
            self.context.i32_type().fn_type(
                &[self
                    .context
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .into()],
                false,
            ),
        );
        self.builder().build_call(
            puts,
            &[self
                .builder()
                .build_global_string_ptr(msg, "")
                .as_pointer_value()
                .into()],
            "puts",
        );
    }

    fn intrinsic_exit(&mut self, status: u64) {
        let exit = self.get_llvm_intrinisic(
            "exit",
            self.context
                .void_type()
                .fn_type(&[self.context.i32_type().into()], false),
        );
        self.builder().build_call(
            exit,
            &[self.context.i32_type().const_int(status, true).into()],
            "exit",
        );
    }

    pub fn build_reduce(&mut self, op: &str, value: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        match value {
            BasicValueEnum::ArrayValue(_) => unimplemented!(),
            BasicValueEnum::FloatValue(_) => unimplemented!(),
            BasicValueEnum::IntValue(i) => i,
            BasicValueEnum::PointerValue(_) => unimplemented!(),
            BasicValueEnum::StructValue(_) => unimplemented!(),
            BasicValueEnum::VectorValue(v) => {
                let fn_type = v
                    .get_type()
                    .get_element_type()
                    .fn_type(&[value.get_type()], false);
                let reduce_fn = self.get_llvm_intrinisic(
                    &format!(
                        "llvm.vector.reduce.{}.{}",
                        op,
                        llvm_intrinsic_type_name(value.get_type())
                    ),
                    fn_type,
                );
                self.builder()
                    .build_call(reduce_fn, &[value], &format!("reduce_{}", op))
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value()
            }
        }
    }

    pub fn build_conditional(
        //<V: PhiMergeable<'ctx>>(
        &mut self,
        cond_value: IntValue<'ctx>,
        build_then: impl FnOnce(&mut Self),
        build_else: impl FnOnce(&mut Self),
        // build_then: impl FnOnce(&mut Self) -> V,
        // build_else: impl FnOnce(&mut Self) -> V,
        // ) -> V {
    ) {
        let then_bb = self.append_basic_block("then");
        let else_bb = self.append_basic_block("else");
        let merge_bb = self.append_basic_block("endif");

        self.builder().build_switch(
            cond_value,
            then_bb,
            &[(cond_value.get_type().const_zero(), else_bb)],
        );

        self.builder().position_at_end(then_bb);
        let value_then = build_then(self);
        let then_end_bb = self.current_block();
        let then_needs_terminator = self.needs_terminator();
        if then_needs_terminator {
            self.builder().build_unconditional_branch(merge_bb);
        }

        self.builder().position_at_end(else_bb);
        let value_else = build_else(self);
        let else_end_bb = self.current_block();
        let else_needs_terminator = self.needs_terminator();
        if else_needs_terminator {
            self.builder().build_unconditional_branch(merge_bb);
        }

        self.builder().position_at_end(merge_bb);
        // let ret = match (then_needs_terminator, else_needs_terminator) {
        //     (true, false) => value_if_true,
        //     (false, true) => value_if_false,
        //     _ => PhiMergeable::merge(
        //         value_if_true,
        //         value_if_false,
        //         if_true_end_bb,
        //         if_false_end_bb,
        //         self,
        //     ),
        // };
        // Ok(ret)
    }
}
