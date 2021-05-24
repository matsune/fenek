use crate::ctx::*;
use crate::wrap::*;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::types::BasicType;
use inkwell::values::{BasicValueEnum, FunctionValue, IntMathValue, IntValue};
use inkwell::AddressSpace;
use inkwell::{FloatPredicate, IntPredicate};
use types::ty;

pub struct FnBuilder<'ctx, 'codegen> {
    ctx: ModuleCtx<'ctx, 'codegen>,
    function: Function<'ctx>,
}

impl<'ctx, 'codegen> FnBuilder<'ctx, 'codegen> {
    pub fn new(ctx: ModuleCtx<'ctx, 'codegen>, function: Function<'ctx>) -> Self {
        Self { ctx, function }
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
        if fun.def.ty.as_fun().ret.is_void() && !fun.block.is_terminated() {
            self.builder().build_return(None);
        }
    }

    fn build_fun_args(&mut self, fun: &hir::Fun) {
        for (idx, ty) in fun.def.ty.as_fun().args.iter().enumerate() {
            let name = fun.args[idx].raw.clone();
            let param = self.fn_value().get_nth_param(idx as u32).unwrap();
            let ptr = self.builder().build_alloca(param.get_type(), &name);
            self.builder().build_store(ptr, param);
            self.function.var_map.insert(
                fun.args[idx].def.id,
                Variable::new(name, ty.clone(), true, ptr),
            );
        }
    }

    fn build_block(&mut self, block: &hir::Block) {
        for stmt in block.stmts.iter() {
            self.build_stmt(stmt);
        }
    }

    fn build_stmt(&mut self, stmt: &hir::Stmt) {
        match stmt {
            hir::Stmt::VarDecl(var_decl) => {
                let name = &var_decl.name.raw;
                let val = self.build_expr(&var_decl.init).unwrap();
                let ptr = self
                    .builder()
                    .build_alloca(llvm_basic_ty(&self.ctx.context, &var_decl.def.ty), &name);
                self.builder().build_store(ptr, val);
                self.function.var_map.insert(
                    var_decl.def.id,
                    Variable::new(name.clone(), var_decl.def.ty.clone(), false, ptr),
                );
            }
            hir::Stmt::Ret(ret) => match &ret.expr {
                Some(expr) => {
                    let val = self.build_expr(&expr).unwrap();
                    self.builder().build_return(Some(match &val {
                        BasicValueEnum::ArrayValue(value) => value,
                        BasicValueEnum::IntValue(value) => value,
                        BasicValueEnum::FloatValue(value) => value,
                        BasicValueEnum::PointerValue(value) => value,
                        BasicValueEnum::StructValue(value) => value,
                        BasicValueEnum::VectorValue(value) => value,
                    }));
                }
                None => {
                    self.builder().build_return(None);
                }
            },
            hir::Stmt::Assign(assign) => {
                let left = self.build_expr_left(&assign.left);
                let right = self.build_expr(&assign.right).unwrap();
                self.builder().build_store(left.into_pointer_value(), right);
            }
            hir::Stmt::Expr(expr) => {
                self.build_expr(&expr);
            }
            hir::Stmt::IfStmt(if_stmt) => {
                self.build_if(&if_stmt);
            }
        }
    }

    fn build_if(&mut self, if_stmt: &hir::IfStmt) {
        match &if_stmt.expr {
            Some(expr) => {
                let cond_value = self.build_expr(&expr).unwrap().into_int_value();
                match &if_stmt.else_if {
                    Some(else_if) => {
                        self.build_conditional(
                            cond_value,
                            |this| {
                                // then
                                this.build_block(&if_stmt.block);
                            },
                            |this| {
                                // else
                                this.build_if(&else_if);
                            },
                        );
                    }
                    None => {
                        let then_bb = self.append_basic_block("then");
                        let end_bb = self.append_basic_block("endif");

                        self.builder()
                            .build_conditional_branch(cond_value, then_bb, end_bb);

                        self.builder().position_at_end(then_bb);
                        self.build_block(&if_stmt.block);
                        let then_end_bb = self.current_block();
                        let then_needs_terminator = then_end_bb.get_terminator().is_none();
                        if then_needs_terminator {
                            self.builder().build_unconditional_branch(end_bb);
                        }

                        self.builder().position_at_end(end_bb);
                    }
                }
            }
            None => {
                self.build_block(&if_stmt.block);
            }
        }
    }

    fn build_expr_left(&self, expr: &hir::Expr) -> BasicValueEnum<'ctx> {
        match expr {
            hir::Expr::Path(ident) => self.function.var_map.get(&ident.def.id).unwrap().ptr.into(),
            hir::Expr::DerefExpr(deref_expr) => {
                let ptr = self.build_expr_left(&deref_expr.expr).into_pointer_value();
                self.builder().build_load(ptr, "")
            }
            hir::Expr::RefExpr(ref_expr) => {
                let expr = self.build_expr_left(&ref_expr.expr);
                let ptr = self.builder().build_alloca(expr.get_type(), "");
                self.builder().build_store(ptr, expr);
                ptr.into()
            }
            _ => unreachable!(),
        }
    }

    fn build_expr(&mut self, expr: &hir::Expr) -> Option<BasicValueEnum<'ctx>> {
        let v = match expr {
            hir::Expr::Lit(lit) => {
                let basic_ty = llvm_basic_ty(&self.ctx.context, &lit.ty);
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
                let ptr = self.function.var_map.get(&ident.def.id).unwrap().ptr;
                self.builder().build_load(ptr, &ident.raw)
            }
            hir::Expr::Call(call) => {
                let mut args = Vec::new();
                for arg in &call.args {
                    args.push(self.build_expr(&arg).unwrap());
                }
                let fn_value = self.ctx.def_fn_value_map.get(&call.def.id).unwrap();
                self.builder()
                    .build_call(*fn_value, &args, "")
                    .try_as_basic_value()
                    .left()?
            }
            hir::Expr::Binary(binary) => {
                let lhs_ty = binary.lhs.get_type();
                let lhs = self.build_expr(&binary.lhs).unwrap();
                let rhs = self.build_expr(&binary.rhs).unwrap();
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
                let expr = self.build_expr(&neg_expr.expr).unwrap();
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
                let expr = self.build_expr(&not_expr.expr).unwrap();
                match not_expr.get_type() {
                    ty::Type::Bool => self
                        .builder()
                        .build_int_neg(expr.into_int_value(), "neg")
                        .into(),
                    _ => unreachable!(),
                }
            }
            hir::Expr::RefExpr(ref_expr) => self.build_expr_left(&ref_expr.expr),
            hir::Expr::DerefExpr(ref_expr) => {
                let value = self.build_expr(&ref_expr.expr).unwrap();
                self.builder().build_load(value.into_pointer_value(), "")
            }
        };
        Some(v)
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
            llvm_intrinsic_type_name(arg_type)
        );
        let bool_type;
        if arg_type.is_vector_type() {
            bool_type = self
                .ctx
                .context
                .bool_type()
                .vec_type(arg_type.into_vector_type().get_size())
                .into();
        } else {
            bool_type = self.ctx.context.bool_type().into();
        }
        let intrinsic_return_type = self.ctx.context.struct_type(&[arg_type, bool_type], false);
        let intrinsic_fn_type = intrinsic_return_type.fn_type(&[arg_type; 2], false);
        let intrinsic_fn = get_llvm_intrinsic(&self.ctx.module, &intrinsic_name, intrinsic_fn_type);
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
            |this| {
                this.intrinsic_puts(
                    &this.builder(),
                    &format!("attempt to {} with overflow", name),
                );
                this.intrinsic_exit(&this.builder(), 1);
            },
            // Otherwise proceed.
            |_| (),
        );

        result_value
    }

    fn intrinsic_puts(&self, builder: &Builder<'ctx>, msg: &str) {
        let puts = get_llvm_intrinsic(
            self.ctx.module,
            "puts",
            self.ctx.context.i32_type().fn_type(
                &[self
                    .ctx
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
        let exit = get_llvm_intrinsic(
            self.ctx.module,
            "exit",
            self.ctx
                .context
                .void_type()
                .fn_type(&[self.ctx.context.i32_type().into()], false),
        );
        builder.build_call(
            exit,
            &[self.ctx.context.i32_type().const_int(status, true).into()],
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
                let reduce_fn = get_llvm_intrinsic(
                    self.ctx.module,
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

    fn append_basic_block(&self, name: &str) -> BasicBlock<'ctx> {
        self.ctx.context.append_basic_block(*self.fn_value(), name)
    }

    fn build_conditional<V: PhiMergeable<'ctx>>(
        &mut self,
        cond_value: IntValue<'ctx>,
        build_then: impl FnOnce(&mut Self) -> V,
        build_else: impl FnOnce(&mut Self) -> V,
    ) -> V {
        let then_bb = self.append_basic_block("then");
        let else_bb = self.append_basic_block("else");
        let mut merge_bb: Option<BasicBlock<'ctx>> = None;

        self.builder()
            .build_conditional_branch(cond_value, then_bb, else_bb);

        self.builder().position_at_end(then_bb);
        let value_then = build_then(self);
        let then_end_bb = self.current_block();
        let then_needs_terminator = then_end_bb.get_terminator().is_none();
        if then_needs_terminator {
            if merge_bb.is_none() {
                merge_bb = Some(self.append_basic_block("endif"));
            }
            self.builder().build_unconditional_branch(merge_bb.unwrap());
        }

        self.builder().position_at_end(else_bb);
        let value_else = build_else(self);
        let else_end_bb = self.current_block();
        let else_needs_terminator = else_end_bb.get_terminator().is_none();
        if else_needs_terminator {
            if merge_bb.is_none() {
                merge_bb = Some(self.append_basic_block("endif"));
            }
            self.builder().build_unconditional_branch(merge_bb.unwrap());
        }

        if let Some(merge_bb) = merge_bb {
            self.builder().position_at_end(merge_bb);
        }
        let ret = match (then_needs_terminator, else_needs_terminator) {
            (true, false) => value_then,
            (false, true) => value_else,
            _ => PhiMergeable::merge(
                value_then,
                value_else,
                then_end_bb,
                else_end_bb,
                self.builder(),
            ),
        };
        ret
    }
}
