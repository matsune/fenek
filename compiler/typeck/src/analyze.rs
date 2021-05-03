use super::arena::*;
use crate::infer::*;
use crate::scope::*;
use error::{CompileError, Result, TypeCkError};
use hir::def::*;
use pos::{Pos, SrcFile};
use std::collections::{HashMap, HashSet};

pub type NodeMap<T> = HashMap<ast::NodeId, T>;

fn compile_error(pos: Pos, typeck_err: TypeCkError) -> CompileError {
    CompileError::new(pos, Box::new(typeck_err))
}

pub struct TyAnalyzer<'src, 'infer> {
    src: &'src SrcFile,
    ty_arena: &'infer InferTyArena<'infer>,
    def_arena: &'infer DefArena<&'infer InferTy<'infer>>,
    node_ty_map: NodeMap<&'infer InferTy<'infer>>,
    node_def_map: NodeMap<&'infer Def<&'infer InferTy<'infer>>>,
    scopes: Scopes<'infer>,
}

impl<'src, 'infer> TyAnalyzer<'src, 'infer> {
    pub fn new(
        src: &'src SrcFile,
        ty_arena: &'infer InferTyArena<'infer>,
        def_arena: &'infer DefArena<&'infer InferTy<'infer>>,
    ) -> Self {
        TyAnalyzer {
            src,
            ty_arena,
            def_arena,
            node_ty_map: NodeMap::new(),
            node_def_map: NodeMap::new(),
            scopes: Scopes::default(),
        }
    }

    fn get_type_from_ty(&self, ty: &ast::Ty) -> Option<&'infer InferTy<'infer>> {
        let infer_ty = match &ty.kind {
            ast::TyKind::Basic(tok) => match tok.raw.as_str() {
                "i8" => self.ty_arena.alloc_i8(),
                "i16" => self.ty_arena.alloc_i16(),
                "i32" => self.ty_arena.alloc_i32(),
                "i64" => self.ty_arena.alloc_i64(),
                "f32" => self.ty_arena.alloc_f32(),
                "f64" => self.ty_arena.alloc_f64(),
                "bool" => self.ty_arena.alloc_bool(),
                // "string" => self.ty_arena.alloc_string(),
                "void" => self.ty_arena.alloc_void(),
                _ => return None,
            },
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
            None => self.ty_arena.alloc_void(),
        };

        Ok(self
            .def_arena
            .alloc(self.ty_arena.alloc_fun(arg_tys, ret_ty), false))
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
                keyword: _,
                name,
                init,
            } => {
                if self.scopes.lookup_var(&name.raw, true).is_some() {
                    return Err(compile_error(
                        self.src.pos_from_offset(name.offset),
                        TypeCkError::AlreadyDefinedVariable(name.raw.clone()),
                    ));
                }
                let init_ty = self.analyze_expr(&init)?;
                let var_ty = self.ty_arena.alloc_var();
                init_ty.set_prune(var_ty);
                let def = self.def_arena.alloc(init_ty, true);
                self.scopes.insert(name.raw.clone(), def);
                self.node_def_map.insert(stmt.id, def);
            }
            ast::StmtKind::Ret { keyword: _, expr } => {
                let ty = match &expr {
                    Some(expr) => self.analyze_expr(&expr)?,
                    None => self.ty_arena.alloc_void(),
                };
                ty.set_prune(current_fn_ret_ty);
            }
            ast::StmtKind::Assign(left, right) => {
                let left_ty = self.analyze_expr(&left)?;
                let right_ty = self.analyze_expr(&right)?;
                left_ty.set_prune(right_ty);
            }
            ast::StmtKind::Empty(_) => {}
        };
        Ok(())
    }

    fn analyze_expr(&mut self, expr: &ast::Expr) -> Result<&'infer InferTy<'infer>> {
        let ty = match &expr.kind {
            ast::ExprKind::Lit(lit) => match lit.kind {
                ast::LitKind::Int(_) => self.ty_arena.alloc_int_lit(),
                ast::LitKind::Float => self.ty_arena.alloc_float_lit(),
                ast::LitKind::Bool => self.ty_arena.alloc_bool(),
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
                let ty = self.ty_arena.alloc_var();
                let arg_tys = &def.ty.kind.as_fun().arg_tys;
                for (idx, arg) in args.iter().enumerate() {
                    let ty = arg_tys[idx];
                    let arg_ty = self.analyze_expr(&arg)?;
                    arg_ty.set_prune(ty);
                }
                ty.set_prune(&def.ty.kind.as_fun().ret_ty);
                ty
            }
            ast::ExprKind::Binary(_, lhs, rhs) => {
                let binary_ty = self.ty_arena.alloc_var();
                let lhs_ty = self.analyze_expr(&lhs)?;
                let rhs_ty = self.analyze_expr(&rhs)?;
                lhs_ty.set_prune(binary_ty);
                rhs_ty.set_prune(binary_ty);
                binary_ty
            }
            ast::ExprKind::Unary(_, expr) => {
                let unary_ty = self.ty_arena.alloc_var();
                let expr_ty = self.analyze_expr(&expr)?;
                expr_ty.set_prune(unary_ty);
                unary_ty
            }
        };
        self.node_ty_map.insert(expr.id, ty);
        Ok(ty)
    }
}
