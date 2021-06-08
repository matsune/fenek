use super::arena::*;
use crate::scope::*;
use ast::Node;
use error::{CompileError, Result, TypeCkError};
use hir::def::*;
use pos::Pos;
use std::collections::{HashMap, HashSet};
use types::infer::*;
use types::solve::Solver;

pub type NodeMap<T> = HashMap<ast::NodeId, T>;

fn compile_error(pos: Pos, typeck_err: TypeCkError) -> CompileError {
    CompileError::new(pos, Box::new(typeck_err))
}

pub struct TyAnalyzer<'lower> {
    solver: &'lower Solver<'lower>,
    def_arena: &'lower DefArena<&'lower InferTy<'lower>>,
    node_ty_map: NodeMap<&'lower InferTy<'lower>>,
    node_def_map: NodeMap<&'lower Def<&'lower InferTy<'lower>>>,
    scopes: Scopes<'lower>,
}

impl<'lower> TyAnalyzer<'lower> {
    pub fn new(
        solver: &'lower Solver<'lower>,
        def_arena: &'lower DefArena<&'lower InferTy<'lower>>,
    ) -> Self {
        TyAnalyzer {
            solver,
            def_arena,
            node_ty_map: NodeMap::new(),
            node_def_map: NodeMap::new(),
            scopes: Scopes::default(),
        }
    }

    fn get_type_from_ty(&self, ty: &ast::Ty) -> &'lower InferTy<'lower> {
        match &ty.kind {
            ast::TyKind::Raw(tok) => match tok.raw.as_str() {
                "i8" => self.solver.arena.alloc_i8(),
                "i16" => self.solver.arena.alloc_i16(),
                "i32" => self.solver.arena.alloc_i32(),
                "i64" => self.solver.arena.alloc_i64(),
                "f32" => self.solver.arena.alloc_f32(),
                "f64" => self.solver.arena.alloc_f64(),
                "bool" => self.solver.arena.alloc_bool(),
                // "string" => self.solver.arena.alloc_string(),
                "void" => self.solver.arena.alloc_void(),
                _ => self.solver.arena.alloc_struct(tok.raw),
            },
            ast::TyKind::Ptr(ty) => self.solver.arena.alloc_ref(self.get_type_from_ty(ty)),
        }
    }

    pub fn analyze_module(
        mut self,
        module: &ast::Module,
    ) -> Result<(
        NodeMap<&'lower InferTy<'lower>>,
        NodeMap<&'lower Def<&'lower InferTy<'lower>>>,
    )> {
        for fun in &module.funs {
            if self.scopes.lookup_fun(&fun.name.raw).is_some() {
                return Err(compile_error(
                    fun.name.pos,
                    TypeCkError::AlreadyDefinedFun(fun.name.raw.clone()),
                ));
            }
            let fun_def = self.make_fun_def(&fun)?;
            self.scopes.insert(fun.name.raw.clone(), fun_def);
            self.node_def_map.insert(fun.id, fun_def);
        }

        for fun in &module.funs {
            self.analyze_fun(&fun)?;
        }

        Ok((self.node_ty_map, self.node_def_map))
    }

    fn make_fun_def(&mut self, fun: &ast::Fun) -> Result<&'lower Def<&'lower InferTy<'lower>>> {
        let mut arg_muts = Vec::new();
        let mut arg_tys = Vec::new();
        let mut arg_names = HashSet::new();
        for arg in &fun.args {
            arg_muts.push(arg.is_mut());

            let arg_name = &arg.name.raw;
            if arg_names.contains(arg_name) {
                return Err(compile_error(
                    arg.name.pos,
                    TypeCkError::AlreadyDefinedVariable(arg_name.clone()),
                ));
            }
            arg_names.insert(arg_name);
            let arg_ty = self.get_type_from_ty(&arg.ty).ok_or_else(|| {
                compile_error(arg.ty.pos(), TypeCkError::UndefinedType(arg.ty.to_string()))
            })?;
            if !arg_ty.kind.is_variable() {
                return Err(compile_error(arg.ty.pos(), TypeCkError::InvalidType));
            }
            arg_tys.push(arg_ty);
            self.node_ty_map.insert(arg.id, arg_ty);
        }

        let (is_ret_mut, ret_ty) = match &fun.ret_ty {
            Some(ret_ty) => {
                let ret_ty_id = ret_ty.ty.id;
                let ty = self.get_type_from_ty(&ret_ty.ty).ok_or_else(|| {
                    compile_error(
                        ret_ty.pos(),
                        TypeCkError::UndefinedType(ret_ty.ty.to_string()),
                    )
                })?;
                self.node_ty_map.insert(ret_ty_id, ty);
                (ret_ty.is_mut(), ty)
            }
            None => (false, self.solver.arena.alloc_void()),
        };

        Ok(self.def_arena.alloc(
            self.solver.arena.alloc_fun(arg_tys, ret_ty),
            DefKind::Fn(arg_muts, is_ret_mut),
        ))
    }

    fn analyze_fun(&mut self, fun: &ast::Fun) -> Result<()> {
        let fun_def = self.scopes.lookup_fun(&fun.name.raw).unwrap();
        self.scopes.push_scope();

        for (idx, arg) in fun.args.iter().enumerate() {
            let ty = fun_def.ty.kind.as_fun().arg_tys[idx];
            let def = self.def_arena.alloc(
                ty,
                DefKind::Var {
                    is_mut: arg.is_mut(),
                },
            );
            self.scopes.insert(arg.name.raw.clone(), def);
            self.node_def_map.insert(arg.id, def);
        }

        self.analyze_block(&fun.block, &fun_def.ty.kind.as_fun().ret_ty)?;

        self.scopes.pop_scope();
        Ok(())
    }

    fn analyze_block(&mut self, block: &ast::Block, ret_ty: &'lower InferTy<'lower>) -> Result<()> {
        for stmt in &block.stmts {
            self.analyze_stmt(&stmt, &ret_ty)?;
        }
        Ok(())
    }

    fn analyze_stmt(
        &mut self,
        stmt: &ast::Stmt,
        current_fn_ret_ty: &'lower InferTy<'lower>,
    ) -> Result<()> {
        match &stmt {
            ast::Stmt::Empty(_) => Ok(()),
            ast::Stmt::Expr(expr) => self.analyze_expr(&expr).map(|_| ()),
            ast::Stmt::VarDecl(var_decl) => self.analyze_var_decl(&var_decl),
            ast::Stmt::Ret(ret_stmt) => self.analyze_ret_stmt(&ret_stmt, current_fn_ret_ty),
            ast::Stmt::Assign(assign) => self.analyze_assign(&assign),
            ast::Stmt::If(if_stmt) => self.analyze_if_stmt(&if_stmt, current_fn_ret_ty),
        }
    }

    fn analyze_var_decl(&mut self, var_decl: &ast::VarDecl) -> Result<()> {
        if self.scopes.lookup_var(&var_decl.name.raw, true).is_some() {
            return Err(compile_error(
                var_decl.name.pos,
                TypeCkError::AlreadyDefinedVariable(var_decl.name.raw.clone()),
            ));
        }
        let init_ty = self.analyze_expr(&var_decl.init)?;
        let var_ty = match &var_decl.ty {
            Some(ty) => {
                let pos = ty.pos();
                let ty = self.get_type_from_ty(&ty).ok_or_else(|| {
                    compile_error(pos, TypeCkError::UndefinedType(ty.to_string()))
                })?;
                if !ty.kind.is_variable() {
                    return Err(compile_error(pos, TypeCkError::InvalidType));
                }
                ty
            }
            None => self.solver.arena.alloc_var(),
        };
        self.solver
            .bind(var_ty, init_ty)
            .map_err(|err| CompileError::new(var_decl.name.pos, err.into()))?;
        let def = self.def_arena.alloc(
            init_ty,
            DefKind::Var {
                is_mut: var_decl.is_mut(),
            },
        );
        self.scopes.insert(var_decl.name.raw.clone(), def);
        self.node_def_map.insert(var_decl.id, def);
        Ok(())
    }

    fn analyze_ret_stmt(
        &mut self,
        ret_stmt: &ast::RetStmt,
        current_fn_ret_ty: &'lower InferTy<'lower>,
    ) -> Result<()> {
        let ty = match &ret_stmt.expr {
            Some(expr) => self.analyze_expr(&expr)?,
            None => self.solver.arena.alloc_void(),
        };
        let pos = ret_stmt
            .expr
            .as_ref()
            .map(|e| e.pos())
            .unwrap_or(ret_stmt.pos());
        self.solver
            .bind(ty, current_fn_ret_ty)
            .map_err(|err| CompileError::new(pos, err.into()))?;
        Ok(())
    }

    fn analyze_assign(&mut self, assign: &ast::Assign) -> Result<()> {
        let left_ty = self.analyze_expr(&assign.left)?;
        let right_ty = self.analyze_expr(&assign.right)?;
        self.solver
            .bind(left_ty, right_ty)
            .map_err(|err| CompileError::new(assign.pos(), err.into()))?;
        Ok(())
    }

    fn analyze_if_stmt(
        &mut self,
        if_stmt: &ast::IfStmt,
        current_fn_ret_ty: &'lower InferTy<'lower>,
    ) -> Result<()> {
        let expr_ty = self.analyze_expr(&if_stmt.expr.as_ref().unwrap())?;
        self.solver
            .bind(expr_ty, self.solver.arena.alloc_bool())
            .map_err(|err| CompileError::new(if_stmt.expr.as_ref().unwrap().pos(), err.into()))?;
        self.analyze_block(&if_stmt.block, &current_fn_ret_ty)?;
        if let Some(else_if) = &if_stmt.else_if {
            self.analyze_else(&else_if, &current_fn_ret_ty)?;
        }
        Ok(())
    }

    fn analyze_else(
        &mut self,
        else_stmt: &ast::IfStmt,
        current_fn_ret_ty: &'lower InferTy<'lower>,
    ) -> Result<()> {
        if let Some(expr) = &else_stmt.expr {
            let expr_ty = self.analyze_expr(&expr)?;
            self.solver
                .bind(expr_ty, self.solver.arena.alloc_bool())
                .map_err(|err| CompileError::new(expr.pos(), err.into()))?;
        }
        self.analyze_block(&else_stmt.block, &current_fn_ret_ty)?;
        match &else_stmt.else_if {
            Some(else_if) => self.analyze_else(&else_if, current_fn_ret_ty)?,
            None => {}
        }
        Ok(())
    }

    fn analyze_expr(&mut self, expr: &ast::Expr) -> Result<&'lower InferTy<'lower>> {
        let ty = match &expr {
            ast::Expr::Lit(lit) => self.analyze_lit(lit),
            ast::Expr::Path(path) => self.analyze_path(path)?,
            ast::Expr::Call(call) => self.analyze_call(call)?,
            ast::Expr::Binary(binary) => self.analyze_binary(binary)?,
            ast::Expr::Unary(unary) => self.analyze_unary(unary)?,
        };
        self.node_ty_map.insert(expr.id(), ty);
        Ok(ty)
    }

    fn analyze_lit(&mut self, lit: &ast::Lit) -> &'lower InferTy<'lower> {
        match lit.kind {
            ast::LitKind::Int { base: _ } => self.solver.arena.alloc_int_lit(),
            ast::LitKind::Float => self.solver.arena.alloc_float_lit(),
            ast::LitKind::Bool => self.solver.arena.alloc_bool(),
            ast::LitKind::String => unimplemented!(),
        }
    }

    fn analyze_path(&mut self, path: &ast::Path) -> Result<&'lower InferTy<'lower>> {
        let def = self
            .scopes
            .lookup_var(&path.ident.raw, false)
            .ok_or_else(|| {
                compile_error(
                    path.pos(),
                    TypeCkError::UndefinedVariable(path.ident.raw.clone()),
                )
            })?;
        self.node_def_map.insert(path.id, def);
        Ok(def.ty)
    }

    fn analyze_call(&mut self, call: &ast::Call) -> Result<&'lower InferTy<'lower>> {
        let def = self
            .scopes
            .lookup_fun(&call.path.ident.raw)
            .ok_or_else(|| {
                compile_error(
                    call.path.pos(),
                    TypeCkError::UndefinedFun(call.path.ident.raw.clone()),
                )
            })?;
        self.node_def_map.insert(call.path.id, def);
        if def.ty.kind.as_fun().arg_tys.len() != call.args.len() {
            return Err(compile_error(call.pos(), TypeCkError::InvalidArgsCount));
        }
        let ty = self.solver.arena.alloc_var();
        let arg_tys = &def.ty.kind.as_fun().arg_tys;
        for (idx, arg) in call.args.iter().enumerate() {
            let ty = arg_tys[idx];
            let arg_ty = self.analyze_expr(&arg)?;
            self.solver
                .bind(arg_ty, ty)
                .map_err(|err| CompileError::new(arg.pos(), err.into()))?;
        }
        self.solver
            .bind(ty, &def.ty.kind.as_fun().ret_ty)
            .map_err(|err| CompileError::new(call.pos(), err.into()))?;
        Ok(ty)
    }

    fn analyze_binary(&mut self, binary: &ast::Binary) -> Result<&'lower InferTy<'lower>> {
        let lhs_ty = self.analyze_expr(&binary.lhs)?;
        let rhs_ty = self.analyze_expr(&binary.rhs)?;
        let ty = match binary.op.kind {
            ast::BinOpKind::Lt | ast::BinOpKind::Gt | ast::BinOpKind::Le | ast::BinOpKind::Ge => {
                self.solver
                    .bind(lhs_ty, rhs_ty)
                    .map_err(|err| CompileError::new(binary.lhs.pos(), err.into()))?;
                self.solver.arena.alloc_bool()
            }
            _ => self
                .solver
                .bind(lhs_ty, rhs_ty)
                .map_err(|err| CompileError::new(binary.lhs.pos(), err.into()))?,
        };
        Ok(ty)
    }

    fn analyze_unary(&mut self, unary: &ast::Unary) -> Result<&'lower InferTy<'lower>> {
        let expr_ty = self.analyze_expr(&unary.expr)?;
        let ty = match unary.op.kind {
            ast::UnOpKind::Ref => self.solver.arena.alloc_ref(expr_ty),
            ast::UnOpKind::Deref => self.solver.arena.alloc_deref(expr_ty),
            _ => {
                let ty = self.solver.arena.alloc_var();
                self.solver
                    .bind(ty, expr_ty)
                    .map_err(|err| CompileError::new(unary.expr.pos(), err.into()))?
            }
        };
        Ok(ty)
    }
}
