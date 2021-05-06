use super::arena::*;
use crate::scope::*;
use error::{CompileError, Result, TypeCkError};
use hir::def::*;
use pos::{Pos, SrcFile};
use std::collections::{HashMap, HashSet};
use types::infer::*;
use types::solve::Solver;

pub type NodeMap<T> = HashMap<ast::NodeId, T>;

fn compile_error(pos: Pos, typeck_err: TypeCkError) -> CompileError {
    CompileError::new(pos, Box::new(typeck_err))
}

pub struct TyAnalyzer<'src, 'infer> {
    src: &'src SrcFile,
    solver: &'infer Solver<'infer>,
    def_arena: &'infer DefArena<&'infer InferTy<'infer>>,
    node_ty_map: NodeMap<&'infer InferTy<'infer>>,
    node_def_map: NodeMap<&'infer Def<&'infer InferTy<'infer>>>,
    scopes: Scopes<'infer>,
}

impl<'src, 'infer> TyAnalyzer<'src, 'infer> {
    pub fn new(
        src: &'src SrcFile,
        solver: &'infer Solver<'infer>,
        def_arena: &'infer DefArena<&'infer InferTy<'infer>>,
    ) -> Self {
        TyAnalyzer {
            src,
            solver,
            def_arena,
            node_ty_map: NodeMap::new(),
            node_def_map: NodeMap::new(),
            scopes: Scopes::default(),
        }
    }

    fn get_type_from_ty(&self, ty: &ast::Ty) -> Option<&'infer InferTy<'infer>> {
        let infer_ty = match &ty.kind {
            ast::TyKind::Basic(tok) => match tok.raw.as_str() {
                "i8" => self.solver.arena.alloc_i8(),
                "i16" => self.solver.arena.alloc_i16(),
                "i32" => self.solver.arena.alloc_i32(),
                "i64" => self.solver.arena.alloc_i64(),
                "f32" => self.solver.arena.alloc_f32(),
                "f64" => self.solver.arena.alloc_f64(),
                "bool" => self.solver.arena.alloc_bool(),
                // "string" => self.solver.arena.alloc_string(),
                "void" => self.solver.arena.alloc_void(),
                _ => return None,
            },
            ast::TyKind::Ptr(ty, _) => self.solver.arena.alloc_ptr(self.get_type_from_ty(ty)?),
        };
        Some(infer_ty)
    }

    pub fn analyze_module(
        mut self,
        module: &ast::Module,
    ) -> Result<(
        NodeMap<&'infer InferTy<'infer>>,
        NodeMap<&'infer Def<&'infer InferTy<'infer>>>,
    )> {
        for fun in &module.funs {
            if self.scopes.lookup_fun(&fun.name.raw).is_some() {
                return Err(compile_error(
                    self.src.pos_from_offset(fun.name.offset),
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

    fn make_fun_def(&mut self, fun: &ast::Fun) -> Result<&'infer Def<&'infer InferTy<'infer>>> {
        let mut arg_tys = Vec::new();
        let mut arg_names = HashSet::new();
        for arg in &fun.args {
            let arg_name = &arg.name.raw;
            if arg_names.contains(arg_name) {
                return Err(compile_error(
                    self.src.pos_from_offset(arg.name.offset),
                    TypeCkError::AlreadyDefinedVariable(arg_name.clone()),
                ));
            }
            arg_names.insert(arg_name);
            let arg_ty = self.get_type_from_ty(&arg.ty).ok_or_else(|| {
                compile_error(
                    self.src.pos_from_offset(arg.ty.offset()),
                    TypeCkError::UndefinedType(arg.ty.to_string()),
                )
            })?;
            if !arg_ty.kind.is_variable() {
                return Err(compile_error(
                    self.src.pos_from_offset(arg.ty.offset()),
                    TypeCkError::InvalidType,
                ));
            }
            arg_tys.push(arg_ty);
            self.node_ty_map.insert(arg.id, arg_ty);
        }

        let ret_ty = match &fun.ret_ty {
            Some(ret_ty) => {
                let ret_ty_id = ret_ty.id;
                let ty = self.get_type_from_ty(&ret_ty).ok_or_else(|| {
                    compile_error(
                        self.src.pos_from_offset(ret_ty.offset()),
                        TypeCkError::UndefinedType(ret_ty.to_string()),
                    )
                })?;
                self.node_ty_map.insert(ret_ty_id, ty);
                ty
            }
            None => self.solver.arena.alloc_void(),
        };

        Ok(self
            .def_arena
            .alloc(self.solver.arena.alloc_fun(arg_tys, ret_ty), false))
    }

    fn analyze_fun(&mut self, fun: &ast::Fun) -> Result<()> {
        let fun_def = self.scopes.lookup_fun(&fun.name.raw).unwrap();
        self.scopes.push_scope();

        for (idx, arg) in fun.args.iter().enumerate() {
            let ty = fun_def.ty.kind.as_fun().arg_tys[idx];
            let def = self.def_arena.alloc(ty, false);
            self.scopes.insert(arg.name.raw.clone(), def);
            self.node_def_map.insert(arg.id, def);
        }

        for stmt in &fun.block.stmts {
            self.analyze_stmt(&stmt, &fun_def.ty.kind.as_fun().ret_ty)?;
        }

        self.scopes.pop_scope();
        Ok(())
    }

    fn analyze_stmt(
        &mut self,
        stmt: &ast::Stmt,
        current_fn_ret_ty: &'infer InferTy<'infer>,
    ) -> Result<()> {
        match &stmt.kind {
            ast::StmtKind::Expr(expr) => {
                self.analyze_expr(&expr)?;
            }
            ast::StmtKind::VarDecl {
                keyword,
                name,
                ty,
                init,
            } => {
                if self.scopes.lookup_var(&name.raw, true).is_some() {
                    return Err(compile_error(
                        self.src.pos_from_offset(name.offset),
                        TypeCkError::AlreadyDefinedVariable(name.raw.clone()),
                    ));
                }
                let init_ty = self.analyze_expr(&init)?;
                let var_ty = match ty {
                    Some(ty) => {
                        let offset = ty.offset();
                        let ty = self.get_type_from_ty(&ty).ok_or_else(|| {
                            compile_error(
                                self.src.pos_from_offset(offset),
                                TypeCkError::UndefinedType(ty.to_string()),
                            )
                        })?;
                        if !ty.kind.is_variable() {
                            return Err(compile_error(
                                self.src.pos_from_offset(offset),
                                TypeCkError::InvalidType,
                            ));
                        }
                        ty
                    }
                    None => self.solver.arena.alloc_var(),
                };
                self.solver.bind(var_ty, init_ty).map_err(|err| {
                    CompileError::new(self.src.pos_from_offset(name.offset), err.into())
                })?;
                let def = self.def_arena.alloc(init_ty, keyword.raw == "var");
                self.scopes.insert(name.raw.clone(), def);
                self.node_def_map.insert(stmt.id, def);
            }
            ast::StmtKind::Ret { keyword, expr } => {
                let ty = match &expr {
                    Some(expr) => self.analyze_expr(&expr)?,
                    None => self.solver.arena.alloc_void(),
                };
                let offset = match &expr {
                    Some(expr) => expr.offset(),
                    None => keyword.offset,
                };
                self.solver.bind(ty, current_fn_ret_ty).map_err(|err| {
                    CompileError::new(self.src.pos_from_offset(offset), err.into())
                })?;
            }
            ast::StmtKind::Assign(left, right) => {
                let left_ty = self.analyze_expr(&left)?;
                let right_ty = self.analyze_expr(&right)?;
                self.solver.bind(left_ty, right_ty).map_err(|err| {
                    CompileError::new(self.src.pos_from_offset(left.offset()), err.into())
                })?;
            }
            ast::StmtKind::Empty(_) => {}
        };
        Ok(())
    }

    fn analyze_expr(&mut self, expr: &ast::Expr) -> Result<&'infer InferTy<'infer>> {
        let ty = match &expr.kind {
            ast::ExprKind::Lit(lit) => match lit.kind {
                ast::LitKind::Int(_) => self.solver.arena.alloc_int_lit(),
                ast::LitKind::Float => self.solver.arena.alloc_float_lit(),
                ast::LitKind::Bool => self.solver.arena.alloc_bool(),
                ast::LitKind::String => unimplemented!(),
            },
            ast::ExprKind::Path(tok) => match self.scopes.lookup_var(&tok.raw, false) {
                Some(var_def) => {
                    let ty = var_def.ty;
                    self.node_def_map.insert(expr.id, var_def);
                    ty
                }
                None => {
                    return Err(compile_error(
                        self.src.pos_from_offset(tok.offset),
                        TypeCkError::UndefinedVariable(tok.raw.clone()),
                    ));
                }
            },
            ast::ExprKind::Call(path, args) => {
                let def = self.scopes.lookup_fun(&path.raw).ok_or_else(|| {
                    compile_error(
                        self.src.pos_from_offset(path.offset),
                        TypeCkError::UndefinedFun(path.raw.clone()),
                    )
                })?;
                if def.ty.kind.as_fun().arg_tys.len() != args.len() {
                    return Err(compile_error(
                        self.src.pos_from_offset(path.offset),
                        TypeCkError::InvalidArgsCount,
                    ));
                }
                self.node_def_map.insert(expr.id, def);
                let ty = self.solver.arena.alloc_var();
                let arg_tys = &def.ty.kind.as_fun().arg_tys;
                for (idx, arg) in args.iter().enumerate() {
                    let ty = arg_tys[idx];
                    let arg_ty = self.analyze_expr(&arg)?;
                    self.solver.bind(arg_ty, ty).map_err(|err| {
                        CompileError::new(self.src.pos_from_offset(arg.offset()), err.into())
                    })?;
                }
                self.solver
                    .bind(ty, &def.ty.kind.as_fun().ret_ty)
                    .map_err(|err| {
                        CompileError::new(self.src.pos_from_offset(path.offset), err.into())
                    })?;
                ty
            }
            ast::ExprKind::Binary(_, lhs, rhs) => {
                let binary_ty = self.solver.arena.alloc_var();
                let lhs_ty = self.analyze_expr(&lhs)?;
                let rhs_ty = self.analyze_expr(&rhs)?;
                self.solver.bind(binary_ty, lhs_ty).map_err(|err| {
                    CompileError::new(self.src.pos_from_offset(lhs.offset()), err.into())
                })?;
                self.solver.bind(binary_ty, rhs_ty).map_err(|err| {
                    CompileError::new(self.src.pos_from_offset(rhs.offset()), err.into())
                })?;
                binary_ty
            }
            ast::ExprKind::Unary(op, expr) => {
                let expr_ty = self.analyze_expr(&expr)?;
                match op.op_kind() {
                    ast::UnOpKind::Ref => self.solver.arena.alloc_ptr(expr_ty),
                    ast::UnOpKind::Deref => {
                        let derefed_ty = self.solver.arena.alloc_var();
                        let ptr_ty = self.solver.arena.alloc_ptr(derefed_ty);
                        self.solver.bind(expr_ty, ptr_ty).map_err(|err| {
                            CompileError::new(self.src.pos_from_offset(expr.offset()), err.into())
                        })?;
                        derefed_ty
                    }
                    _ => {
                        let ty = self.solver.arena.alloc_var();
                        self.solver.bind(ty, expr_ty).map_err(|err| {
                            CompileError::new(self.src.pos_from_offset(expr.offset()), err.into())
                        })?;
                        ty
                    }
                }
            }
        };
        self.node_ty_map.insert(expr.id, ty);
        Ok(ty)
    }
}
