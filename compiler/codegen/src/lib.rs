use hir::def::DefId;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntMathValue, IntValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use types::ty;

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

    fn current_block(&self) -> BasicBlock<'ctx> {
        self.builder.get_insert_block().unwrap()
    }

    fn needs_terminator(&self) -> bool {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
    }

    fn set_basic_block(&self, basic_block: BasicBlock<'ctx>) {
        self.builder.position_at_end(basic_block);
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
//         this_bb: BasicBlock<'ctx>,
//         other_bb: BasicBlock<'ctx>,
//         codegen: &mut Codegen<'ctx>,
//     ) -> Self;
// }

// impl<'ctx> PhiMergeable<'ctx> for () {
//     fn merge(
//         self,
//         other: Self,
//         this_bb: BasicBlock<'ctx>,
//         other_bb: BasicBlock<'ctx>,
//         codegen: &mut Codegen<'ctx>,
//     ) -> Self {
//     }
// }

// impl<'ctx> PhiMergeable<'ctx> for BasicValueEnum<'ctx> {
//     fn merge(
//         self,
//         other: Self,
//         this_bb: BasicBlock<'ctx>,
//         other_bb: BasicBlock<'ctx>,
//         codegen: &mut Codegen<'ctx>,
//     ) -> Self {
//         let phi = codegen
//             .builder()
//             .build_phi(self.get_type(), "mergeValuesEndIf");
//         phi.add_incoming(&[(&self, this_bb), (&other, other_bb)]);
//         phi.as_basic_value()
//     }
// }

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    def_function_map: HashMap<DefId, Rc<RefCell<Function<'ctx>>>>,
    machine: TargetMachine,
}

fn get_default_machine() -> TargetMachine {
    Target::initialize_native(&InitializationConfig::default()).unwrap();

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    target
        .create_target_machine(
            &triple,
            &TargetMachine::get_host_cpu_name().to_string(),
            &TargetMachine::get_host_cpu_features().to_string(),
            OptimizationLevel::None,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap()
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        Self {
            context,
            module: context.create_module("fenec"),
            def_function_map: HashMap::new(),
            machine: get_default_machine(),
        }
    }

    pub fn emit_llvm_ir<P: AsRef<std::path::Path>>(&self, path: P) -> Result<(), LLVMString> {
        self.module.print_to_file(path)
    }

    pub fn emit_llvm_bc<P: AsRef<std::path::Path>>(&self, path: P) -> bool {
        self.module.write_bitcode_to_path(path.as_ref())
    }

    pub fn emit_asm<P: AsRef<std::path::Path>>(&self, path: P) -> Result<(), LLVMString> {
        self.machine
            .write_to_file(&self.module, FileType::Assembly, path.as_ref())
    }

    pub fn emit_obj<P: AsRef<std::path::Path>>(&self, path: P) -> Result<(), LLVMString> {
        self.machine
            .write_to_file(&self.module, FileType::Object, path.as_ref())
    }

    fn append_basic_block(&self, function: &Function<'ctx>, name: &str) -> BasicBlock<'ctx> {
        self.context.append_basic_block(function.fn_value, name)
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
                ty::IntType::I8 => self.context.i8_type(),
                ty::IntType::I16 => self.context.i16_type(),
                ty::IntType::I32 => self.context.i32_type(),
                ty::IntType::I64 => self.context.i64_type(),
            }
            .into(),
            ty::Type::Float(float_ty) => match float_ty {
                ty::FloatType::F32 => self.context.f32_type(),
                ty::FloatType::F64 => self.context.f64_type(),
            }
            .into(),
            ty::Type::Bool => self.context.bool_type().into(),
            ty::Type::Ref(ty) => {
                let ty = self.llvm_basic_ty(ty);
                ty.ptr_type(AddressSpace::Generic).into()
            }
            _ => unreachable!(),
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
                ty::IntType::I8 => self.context.i8_type(),
                ty::IntType::I16 => self.context.i16_type(),
                ty::IntType::I32 => self.context.i32_type(),
                ty::IntType::I64 => self.context.i64_type(),
            }
            .fn_type(&param_types, false),
            ty::Type::Float(float_ty) => match float_ty {
                ty::FloatType::F32 => self.context.f32_type(),
                ty::FloatType::F64 => self.context.f64_type(),
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

    pub fn build_module(&mut self, module: hir::Module) {
        for fun in &module.funs {
            let function = self.build_function(
                &fun.name,
                &fun.def.ty.as_fun().ret,
                &fun.def.ty.as_fun().args,
            );
            self.def_function_map
                .insert(fun.def.id, Rc::new(RefCell::new(function)));
        }

        for fun in module.funs {
            self.build_fun(fun);
        }
    }

    pub fn verify(&self) -> Result<(), LLVMString> {
        self.module.verify()
    }

    pub fn build_fun(&mut self, fun: hir::Fun) {
        let function = self.def_function_map.get(&fun.def.id).unwrap().clone();
        // append and set basic block
        let start_bb = self.append_basic_block(&function.borrow(), "start");
        function.borrow().set_basic_block(start_bb);
        // args
        self.build_fun_args(&function, &fun);
        // block
        for stmt in fun.block.stmts.iter() {
            self.build_stmt(&function, stmt);
        }
    }

    fn build_fun_args(&mut self, function: &RefCell<Function<'ctx>>, fun: &hir::Fun) {
        for (idx, ty) in fun.def.ty.as_fun().args.iter().enumerate() {
            let name = fun.args[idx].raw.clone();
            let param = function
                .borrow()
                .fn_value
                .get_nth_param(idx as u32)
                .unwrap();
            let ptr = function
                .borrow()
                .builder
                .build_alloca(param.get_type(), &name);
            function.borrow().builder.build_store(ptr, param);
            function.borrow_mut().var_map.insert(
                fun.args[idx].def.id,
                Variable::new(name, ty.clone(), true, ptr),
            );
        }
    }

    fn build_stmt(&mut self, function: &RefCell<Function<'ctx>>, stmt: &hir::Stmt) {
        match stmt {
            hir::Stmt::VarDecl(var_decl) => {
                let name = &var_decl.name.raw;
                let val = self.build_expr(&function.borrow(), &var_decl.init);
                let ptr = function
                    .borrow()
                    .builder
                    .build_alloca(self.llvm_basic_ty(&var_decl.def.ty), &name);
                function.borrow().builder.build_store(ptr, val);
                function.borrow_mut().var_map.insert(
                    var_decl.def.id,
                    Variable::new(name.clone(), var_decl.def.ty.clone(), false, ptr),
                );
            }
            hir::Stmt::Ret(ret) => match &ret.expr {
                Some(expr) => {
                    let val = self.build_expr(&function.borrow(), &expr);
                    function.borrow().builder.build_return(Some(match &val {
                        BasicValueEnum::ArrayValue(value) => value,
                        BasicValueEnum::IntValue(value) => value,
                        BasicValueEnum::FloatValue(value) => value,
                        BasicValueEnum::PointerValue(value) => value,
                        BasicValueEnum::StructValue(value) => value,
                        BasicValueEnum::VectorValue(value) => value,
                    }));
                }
                None => {
                    function.borrow().builder.build_return(None);
                }
            },
            hir::Stmt::Assign(assign) => {
                let right = self.build_expr(&function.borrow(), &assign.right);
                // let left = self.build_expr(&function.borrow(), &assign.left);
                let left = match &*assign.left {
                    hir::Expr::Path(path) => {
                        let ptr = function.borrow().var_map.get(&path.def.id).unwrap().ptr;
                        if path.def.ty.is_ref() {
                            function
                                .borrow()
                                .builder
                                .build_load(ptr, "")
                                .into_pointer_value()
                        } else {
                            ptr
                        }
                    }
                    _ => unreachable!(),
                };
                function.borrow().builder.build_store(left, right);
            }
            hir::Stmt::Expr(expr) => {
                self.build_expr(&function.borrow(), &expr);
            }
        }
    }

    fn get_ptr(&mut self, function: &Function<'ctx>, expr: &hir::Expr) -> PointerValue<'ctx> {
        match expr {
            hir::Expr::Path(ident) => function.var_map.get(&ident.def.id).unwrap().ptr,
            hir::Expr::Unary(unary) => match unary.op {
                ast::UnOpKind::Ref => self.get_ptr(function, &unary.expr),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn build_expr(&mut self, function: &Function<'ctx>, expr: &hir::Expr) -> BasicValueEnum<'ctx> {
        let is_ref = matches!(expr, hir::Expr::Unary(unary) if unary.op == ast::UnOpKind::Ref);
        let value = match expr {
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
            hir::Expr::Path(ident) => {
                let ptr = self.get_ptr(function, expr);
                if is_ref {
                    ptr.into()
                } else {
                    function.builder.build_load(ptr, &ident.raw)
                }
            }
            hir::Expr::Call(call) => {
                let mut args = Vec::new();
                for arg in &call.args {
                    args.push(self.build_expr(&function, &arg));
                }
                let f = self.def_function_map.get(&call.def.id).unwrap();
                function
                    .builder
                    .build_call(f.borrow().fn_value, &args, "")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            }
            hir::Expr::Binary(binary) => {
                let lhs = self.build_expr(function, &binary.lhs);
                let rhs = self.build_expr(function, &binary.rhs);
                match binary.ty {
                    ty::Type::Int(_) => {
                        let lhs = lhs.into_int_value();
                        let rhs = rhs.into_int_value();
                        match binary.op {
                            ast::BinOpKind::Add => {
                                self.build_checked_int_arithmetic(function, lhs, rhs, "sadd")
                            }
                            ast::BinOpKind::Sub => {
                                self.build_checked_int_arithmetic(function, lhs, rhs, "ssub")
                            }
                            ast::BinOpKind::Mul => {
                                self.build_checked_int_arithmetic(function, lhs, rhs, "smul")
                            }
                            ast::BinOpKind::Div => function
                                .builder
                                .build_int_signed_div(lhs, rhs, "sdiv")
                                .into(),
                        }
                    }
                    ty::Type::Float(_) => {
                        let lhs = lhs.into_float_value();
                        let rhs = rhs.into_float_value();
                        match binary.op {
                            ast::BinOpKind::Add => {
                                function.builder.build_float_add(lhs, rhs, "fadd").into()
                            }
                            ast::BinOpKind::Sub => {
                                function.builder.build_float_sub(lhs, rhs, "fsub").into()
                            }
                            ast::BinOpKind::Mul => {
                                function.builder.build_float_mul(lhs, rhs, "fmul").into()
                            }
                            ast::BinOpKind::Div => {
                                function.builder.build_float_div(lhs, rhs, "fdiv").into()
                            }
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            hir::Expr::Unary(unary) => match unary.op {
                ast::UnOpKind::Neg => {
                    let expr = self.build_expr(function, &unary.expr);
                    match unary.get_type() {
                        ty::Type::Int(_) => function
                            .builder
                            .build_int_neg(expr.into_int_value(), "neg")
                            .into(),
                        ty::Type::Float(_) => function
                            .builder
                            .build_float_neg(expr.into_float_value(), "neg")
                            .into(),
                        _ => unimplemented!(),
                    }
                }
                ast::UnOpKind::Not => {
                    let expr = self.build_expr(function, &unary.expr);
                    match unary.get_type() {
                        ty::Type::Bool => function
                            .builder
                            .build_int_neg(expr.into_int_value(), "neg")
                            .into(),
                        _ => unreachable!(),
                    }
                }
                ast::UnOpKind::Ref => self.get_ptr(function, &unary.expr).into(),
            },
        };
        if !is_ref && value.is_pointer_value() {
            function.builder.build_load(value.into_pointer_value(), "")
        } else {
            value
        }
    }

    fn build_checked_int_arithmetic<T: IntMathValue<'ctx>>(
        &mut self,
        function: &Function<'ctx>,
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

        let return_value = function
            .builder
            .build_call(intrinsic_fn, intrinsic_args, "tmp_checked_result")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_struct_value();

        let result_value = function
            .builder
            .build_extract_value(return_value, 0, "tmp_result")
            .unwrap();
        let is_overflow_vec = function
            .builder
            .build_extract_value(return_value, 1, "tmp_overflow")
            .unwrap();
        let is_overflow = self.build_reduce(&function.builder, "or", is_overflow_vec);

        self.build_conditional(
            function,
            is_overflow,
            // Return an error if there is overflow.
            |this| {
                this.intrinsic_puts(
                    &function.builder,
                    &format!("attempt to {} with overflow", name),
                );
                this.intrinsic_exit(&function.builder, 1);
            },
            // Otherwise proceed.
            |_| (),
        );

        result_value
    }

    fn intrinsic_puts(&mut self, builder: &Builder<'ctx>, msg: &str) {
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
        builder.build_call(
            puts,
            &[builder
                .build_global_string_ptr(msg, "")
                .as_pointer_value()
                .into()],
            "puts",
        );
    }

    fn intrinsic_exit(&mut self, builder: &Builder<'ctx>, status: u64) {
        let exit = self.get_llvm_intrinisic(
            "exit",
            self.context
                .void_type()
                .fn_type(&[self.context.i32_type().into()], false),
        );
        builder.build_call(
            exit,
            &[self.context.i32_type().const_int(status, true).into()],
            "exit",
        );
    }

    pub fn build_reduce(
        &mut self,
        builder: &Builder<'ctx>,
        op: &str,
        value: BasicValueEnum<'ctx>,
    ) -> IntValue<'ctx> {
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
                builder
                    .build_call(reduce_fn, &[value], &format!("reduce_{}", op))
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value()
            }
        }
    }

    fn build_conditional(
        //<V: PhiMergeable<'ctx>>(
        &mut self,
        function: &Function<'ctx>,
        cond_value: IntValue<'ctx>,
        build_then: impl FnOnce(&mut Self),
        build_else: impl FnOnce(&mut Self),
        // build_then: impl FnOnce(&mut Self) -> V,
        // build_else: impl FnOnce(&mut Self) -> V,
        // ) -> V {
    ) {
        let then_bb = self.append_basic_block(function, "then");
        let else_bb = self.append_basic_block(function, "else");
        let merge_bb = self.append_basic_block(function, "endif");

        function.builder.build_switch(
            cond_value,
            then_bb,
            &[(cond_value.get_type().const_zero(), else_bb)],
        );

        function.builder.position_at_end(then_bb);
        let value_then = build_then(self);
        let then_end_bb = function.current_block();
        let then_needs_terminator = function.needs_terminator();
        if then_needs_terminator {
            function.builder.build_unconditional_branch(merge_bb);
        }

        function.builder.position_at_end(else_bb);
        let value_else = build_else(self);
        let else_end_bb = function.current_block();
        let else_needs_terminator = function.needs_terminator();
        if else_needs_terminator {
            function.builder.build_unconditional_branch(merge_bb);
        }

        function.builder.position_at_end(merge_bb);
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
