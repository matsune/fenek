use super::llvm;
use super::module::ModuleBuilder;
use crate::wrap::*;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, FunctionValue, IntMathValue, IntValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::{FloatPredicate, IntPredicate};
use types::ty;

pub struct FnBuilder<'ctx, 'module> {
    module: &'module mut ModuleBuilder<'ctx>,
    function: Function<'ctx>,
}

impl<'ctx, 'module> FnBuilder<'ctx, 'module> {
    pub fn new(module: &'module mut ModuleBuilder<'ctx>, function: Function<'ctx>) -> Self {
        Self { module, function }
    }

    fn fn_value(&self) -> &FunctionValue<'ctx> {
        &self.function.fn_value
    }

    fn builder(&self) -> &Builder<'ctx> {
        &self.function.builder
    }

    fn current_block(&self) -> BasicBlock<'ctx> {
        self.builder().get_insert_block().unwrap()
    }

    pub fn build_fun(mut self, fun: &hir::Fun) {
        self.build_fun_args(&fun);
        self.build_block(&fun.block);
        if fun.def.ty().as_fun().ret.is_void() && !fun.block.is_terminated() {
            self.builder().build_return(None);
        }
    }

    fn build_fun_args(&mut self, fun: &hir::Fun) {
        for (idx, ty) in fun.def.ty().as_fun().args.iter().enumerate() {
            let name = fun.args[idx].raw.clone();
            let param = self.fn_value().get_nth_param(idx as u32).unwrap();
            let ptr = self.builder().build_alloca(param.get_type(), &name);
            self.builder().build_store(ptr, param);
            self.function.var_map.insert(
                fun.args[idx].def.id(),
                Variable::new(name, ty.clone(), true, ptr),
            );
        }
    }

    fn build_block(&mut self, block: &hir::Block) {
        for stmt in block.stmts.iter() {
            self.build_stmt(stmt);
        }
    }

    fn store(&mut self, dest: PointerValue<'ctx>, val: BasicValueEnum<'ctx>) {
        if dest.get_type().get_element_type().is_struct_type() {
            let ptr = self
                .builder()
                .build_bitcast(
                    dest,
                    self.module
                        .context
                        .i8_type()
                        .ptr_type(AddressSpace::Generic),
                    "",
                )
                .into_pointer_value();
            let val = self
                .builder()
                .build_bitcast(
                    val,
                    self.module
                        .context
                        .i8_type()
                        .ptr_type(AddressSpace::Generic),
                    "",
                )
                .into_pointer_value();
            self.builder()
                .build_memcpy(
                    ptr,
                    1,
                    val,
                    1,
                    self.module.context.i32_type().const_int(
                        u64::from(
                            self.module
                                .machine
                                .get_target_data()
                                .get_pointer_byte_size(None),
                        ),
                        true,
                    ),
                )
                .unwrap();
        } else {
            self.builder().build_store(dest, val);
        }
    }

    fn build_stmt(&mut self, stmt: &hir::Stmt) {
        match stmt {
            hir::Stmt::VarDecl(var_decl) => {
                let name = &var_decl.name;
                let val = self.build_expr(&var_decl.init, true);
                let ty = self.module.llvm_basic_type(&var_decl.def.ty());
                let ptr = self.builder().build_alloca(ty, &name);
                self.store(ptr, val);
                self.function.var_map.insert(
                    var_decl.def.id(),
                    Variable::new(name.clone(), var_decl.def.ty().clone(), false, ptr),
                );
            }
            hir::Stmt::Ret(ret) => match &ret.expr {
                Some(expr) => {
                    let val = self.build_expr(&expr, true);
                    self.builder().build_return(Some(&val));
                }
                None => {
                    self.builder().build_return(None);
                }
            },
            hir::Stmt::Assign(assign) => {
                let left = self.build_expr(&assign.left, false);
                let right = self.build_expr(&assign.right, true);
                self.store(left.into_pointer_value(), right);
            }
            hir::Stmt::Expr(expr) => {
                self.build_expr(&expr, true);
            }
            hir::Stmt::IfStmt(if_stmt) => {
                self.build_if(&if_stmt);
            }
        }
    }

    fn build_if(&mut self, if_stmt: &hir::IfStmt) {
        match &if_stmt.expr {
            Some(expr) => {
                let cond_value = self.build_expr(&expr, true).into_int_value();
                match &if_stmt.else_if {
                    Some(else_if) => {
                        // have next else block
                        self.build_then_else_cond(
                            cond_value,
                            |this| {
                                this.build_block(&if_stmt.block);
                            },
                            |this| {
                                this.build_if(&else_if);
                            },
                        );
                    }
                    None => {
                        // no-else if statement
                        self.build_then_cond(cond_value, |this| this.build_block(&if_stmt.block));
                    }
                }
            }
            None => {
                // last else block
                self.build_block(&if_stmt.block);
            }
        }
    }

    fn build_expr(&mut self, expr: &hir::Expr, load_ptr: bool) -> BasicValueEnum<'ctx> {
        match expr {
            hir::Expr::Lit(lit) => {
                let basic_ty = self.module.llvm_basic_type(&lit.ty);
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
                    hir::LitKind::Null => basic_ty.into_pointer_type().const_null().into(),
                }
            }
            hir::Expr::Path(ident) => {
                let ptr = self.function.var_map.get(&ident.def.id()).unwrap().ptr;
                if load_ptr {
                    self.builder().build_load(ptr, &ident.raw)
                } else {
                    ptr.into()
                }
            }
            hir::Expr::Call(call) => {
                let mut args = Vec::new();
                for arg in &call.args {
                    args.push(self.build_expr(&arg, true));
                }
                let fn_value = self
                    .module
                    .def_fn_value_map
                    .get(&call.path.def.id())
                    .unwrap();
                self.builder()
                    .build_call(*fn_value, &args, "")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            }
            hir::Expr::Binary(binary) => {
                let lhs_ty = binary.lhs.get_type();
                let lhs = self.build_expr(&binary.lhs, true);
                let rhs = self.build_expr(&binary.rhs, true);
                match lhs_ty {
                    ty::Type::Int(_) => {
                        let lhs = lhs.into_int_value();
                        let rhs = rhs.into_int_value();
                        match binary.op {
                            ast::BinOpKind::Add => {
                                self.build_checked_int_arithmetic(lhs, rhs, "sadd")
                            }
                            ast::BinOpKind::Sub => {
                                self.build_checked_int_arithmetic(lhs, rhs, "ssub")
                            }
                            ast::BinOpKind::Mul => {
                                self.build_checked_int_arithmetic(lhs, rhs, "smul")
                            }
                            ast::BinOpKind::Div => {
                                self.builder().build_int_signed_div(lhs, rhs, "sdiv").into()
                            }
                            ast::BinOpKind::Lt => self
                                .builder()
                                .build_int_compare(IntPredicate::SLT, lhs, rhs, "")
                                .into(),
                            ast::BinOpKind::Gt => self
                                .builder()
                                .build_int_compare(IntPredicate::SGT, lhs, rhs, "")
                                .into(),
                            ast::BinOpKind::Le => self
                                .builder()
                                .build_int_compare(IntPredicate::SLE, lhs, rhs, "")
                                .into(),
                            ast::BinOpKind::Ge => self
                                .builder()
                                .build_int_compare(IntPredicate::SGE, lhs, rhs, "")
                                .into(),
                        }
                    }
                    ty::Type::Float(_) => {
                        let lhs = lhs.into_float_value();
                        let rhs = rhs.into_float_value();
                        match binary.op {
                            ast::BinOpKind::Add => {
                                self.builder().build_float_add(lhs, rhs, "fadd").into()
                            }
                            ast::BinOpKind::Sub => {
                                self.builder().build_float_sub(lhs, rhs, "fsub").into()
                            }
                            ast::BinOpKind::Mul => {
                                self.builder().build_float_mul(lhs, rhs, "fmul").into()
                            }
                            ast::BinOpKind::Div => {
                                self.builder().build_float_div(lhs, rhs, "fdiv").into()
                            }
                            ast::BinOpKind::Lt => self
                                .builder()
                                .build_float_compare(FloatPredicate::OLT, lhs, rhs, "")
                                .into(),
                            ast::BinOpKind::Gt => self
                                .builder()
                                .build_float_compare(FloatPredicate::OGT, lhs, rhs, "")
                                .into(),
                            ast::BinOpKind::Le => self
                                .builder()
                                .build_float_compare(FloatPredicate::OLE, lhs, rhs, "")
                                .into(),
                            ast::BinOpKind::Ge => self
                                .builder()
                                .build_float_compare(FloatPredicate::OLE, lhs, rhs, "")
                                .into(),
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            hir::Expr::NegExpr(neg_expr) => {
                let expr = self.build_expr(&neg_expr.expr, true);
                match neg_expr.get_type() {
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
            hir::Expr::NotExpr(not_expr) => {
                let expr = self.build_expr(&not_expr.expr, true);
                match not_expr.get_type() {
                    ty::Type::Bool => self
                        .builder()
                        .build_int_neg(expr.into_int_value(), "neg")
                        .into(),
                    _ => unreachable!(),
                }
            }
            hir::Expr::RefExpr(ref_expr) => self.build_expr(&ref_expr.expr, false),
            hir::Expr::DerefExpr(ref_expr) => {
                let ptr_value = self.build_expr(&ref_expr.expr, true).into_pointer_value();
                let is_null = self.builder().build_is_null(ptr_value, "");
                self.build_then_cond(is_null, |this| {
                    this.intrinsic_puts(&this.builder(), "null pointer dereference");
                    this.intrinsic_exit(&this.builder(), 1);
                    this.builder().build_unreachable();
                });
                self.builder().build_load(ptr_value, "")
            }
            hir::Expr::StructInit(struct_init) => {
                let struct_ty = self.module.llvm_basic_type(&struct_init.ty);
                let alloca = self.builder().build_alloca(struct_ty, &struct_init.name);
                for (idx, member) in struct_init.members.iter().enumerate() {
                    let val = self.build_expr(&struct_init.members[idx].expr, false);
                    let ptr = unsafe {
                        self.builder().build_in_bounds_gep(
                            alloca,
                            &[
                                self.module.context.i32_type().const_int(0, true),
                                self.module.context.i32_type().const_int(idx as u64, true),
                            ],
                            &member.name,
                        )
                    };
                    self.store(ptr, val);
                }
                alloca.into()
            }
        }
    }

    fn build_checked_int_arithmetic<T: IntMathValue<'ctx>>(
        &mut self,
        lhs: T,
        rhs: T,
        name: &str,
    ) -> BasicValueEnum<'ctx> {
        let arg_type = lhs.as_basic_value_enum().get_type();

        let intrinsic_name = format!(
            "llvm.{}.with.overflow.{}",
            name,
            llvm::intrinsic_type_name(arg_type)
        );
        let bool_type;
        if arg_type.is_vector_type() {
            bool_type = self
                .module
                .context
                .bool_type()
                .vec_type(arg_type.into_vector_type().get_size())
                .into();
        } else {
            bool_type = self.module.context.bool_type().into();
        }
        let intrinsic_return_type = self
            .module
            .context
            .struct_type(&[arg_type, bool_type], false);
        let intrinsic_fn_type = intrinsic_return_type.fn_type(&[arg_type; 2], false);
        let intrinsic_fn = self
            .module
            .llvm_intrinsic(&intrinsic_name, intrinsic_fn_type);
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

        self.build_then_cond(
            is_overflow,
            // Return an error if there is overflow.
            |this| {
                this.intrinsic_puts(
                    &this.builder(),
                    &format!("attempt to {} with overflow", name),
                );
                this.intrinsic_exit(&this.builder(), 1);
                this.builder().build_unreachable();
            },
        );

        result_value
    }

    fn intrinsic_puts(&self, builder: &Builder<'ctx>, msg: &str) {
        let puts = self.module.llvm_intrinsic(
            "puts",
            self.module.context.i32_type().fn_type(
                &[self
                    .module
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

    fn intrinsic_exit(&self, builder: &Builder<'ctx>, status: u64) {
        let exit = self.module.llvm_intrinsic(
            "exit",
            self.module
                .context
                .void_type()
                .fn_type(&[self.module.context.i32_type().into()], false),
        );
        builder.build_call(
            exit,
            &[self
                .module
                .context
                .i32_type()
                .const_int(status, true)
                .into()],
            "exit",
        );
    }

    fn build_reduce(&self, op: &str, value: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
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
                let reduce_fn = self.module.llvm_intrinsic(
                    &format!(
                        "llvm.vector.reduce.{}.{}",
                        op,
                        llvm::intrinsic_type_name(value.get_type())
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

    fn append_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        self.module
            .context
            .append_basic_block(*self.fn_value(), name)
    }

    // build one way branch
    fn build_then_cond(&mut self, cond_value: IntValue<'ctx>, build_then: impl FnOnce(&mut Self)) {
        let then_bb = self.append_basic_block("then");
        let end_bb = self.append_basic_block("endif");

        self.builder()
            .build_conditional_branch(cond_value, then_bb, end_bb);

        self.builder().position_at_end(then_bb);
        build_then(self);
        let then_end_bb = self.current_block();
        let then_needs_terminator = then_end_bb.get_terminator().is_none();
        if then_needs_terminator {
            self.builder().build_unconditional_branch(end_bb);
        }

        self.builder().position_at_end(end_bb);
    }

    // build two ways branch
    fn build_then_else_cond(
        &mut self,
        cond_value: IntValue<'ctx>,
        build_then: impl FnOnce(&mut Self),
        build_else: impl FnOnce(&mut Self),
    ) {
        let then_bb = self.append_basic_block("then");
        let else_bb = self.append_basic_block("else");

        self.builder()
            .build_conditional_branch(cond_value, then_bb, else_bb);

        self.builder().position_at_end(then_bb);
        build_then(self);
        let then_end_bb = self.current_block();
        let then_has_terminator = then_end_bb.get_terminator().is_some();

        self.builder().position_at_end(else_bb);
        build_else(self);
        let else_end_bb = self.current_block();
        let else_has_terminator = else_end_bb.get_terminator().is_some();

        if then_has_terminator && else_has_terminator {
            return;
        }
        let merge_bb = self.append_basic_block("endif");
        if !then_has_terminator {
            self.builder().position_at_end(then_end_bb);
            self.builder().build_unconditional_branch(merge_bb);
        }
        if !else_has_terminator {
            self.builder().position_at_end(else_end_bb);
            self.builder().build_unconditional_branch(merge_bb);
        }
    }
}
