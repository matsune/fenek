use crate::infer_ty::*;
use crate::scope::*;
use error::{CompileError, Result, TypeCkError};
use hir::def::*;
use hir::ty;
use pos::{Pos, SrcFile};
use std::collections::HashMap;

// https://github.com/rust-lang/rust/issues/22639
fn is_parse_int_overflow_error(e: std::num::ParseIntError) -> bool {
    let pos_overflow_err = "2147483648".parse::<i32>().err().unwrap();
    if e == pos_overflow_err {
        return true;
    }
    let neg_overflow_err = "-2147483649".parse::<i32>().err().unwrap();
    e == neg_overflow_err
}

fn compile_error(pos: Pos, typeck_err: TypeCkError) -> CompileError {
    CompileError::new(pos, Box::new(typeck_err))
}

pub struct TyAnalyzer<'src, 'infer> {
    src: &'src SrcFile,
    ty_arena: &'infer InferTyArena<'infer>,
    pub node_ty_map: HashMap<ast::NodeId, &'infer InferTy<'infer>>,
    pub node_scope_map: HashMap<ast::NodeId, ArenaIdx>,
    pub scopes: Vec<ScopeTable<&'infer InferTy<'infer>>>,
    scope_idx: ArenaIdx,
    def_id: DefId,
    current_fn_ret_ty: Option<&'infer InferTy<'infer>>,
}

impl<'src, 'infer> TyAnalyzer<'src, 'infer> {
    pub fn new(src: &'src SrcFile, ty_arena: &'infer InferTyArena<'infer>) -> Self {
        let mut scopes = Vec::new();
        let scope_idx = scopes.len();
        // add global scope
        scopes.push(ScopeTable::new(scope_idx, None));
        TyAnalyzer {
            src,
            ty_arena,
            node_scope_map: HashMap::new(),
            node_ty_map: HashMap::new(),
            scopes,
            scope_idx,
            def_id: 0,
            current_fn_ret_ty: None,
        }
    }

    fn push_scope(&mut self) {
        let idx = self.scopes.len();
        self.scopes.push(ScopeTable::new(idx, Some(self.scope_idx)));
        self.scope_idx = idx;
    }

    fn pop_scope(&mut self) {
        self.scope_idx = self.scopes[self.scope_idx].parent.unwrap();
    }

    fn get_scope(&self) -> &ScopeTable<&'infer InferTy<'infer>> {
        &self.scopes[self.scope_idx]
    }

    fn get_scope_mut(&mut self) -> &mut ScopeTable<&'infer InferTy<'infer>> {
        &mut self.scopes[self.scope_idx]
    }

    fn gen_def_id(&mut self) -> DefId {
        let id = self.def_id;
        self.def_id += 1;
        id
    }

    fn get_type_from_ty(&self, ty: &ast::Ty) -> Option<&'infer InferTy<'infer>> {
        let infer_ty = match &ty.kind {
            ast::TyKind::Single(tok) => match tok.raw.as_str() {
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

    pub fn analyze_fun(&mut self, fun: &ast::Fun) -> Result<()> {
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
        self.current_fn_ret_ty = Some(ret_ty);
        self.push_scope();
        self.node_scope_map.insert(fun.block.id, self.scope_idx);

        for arg in &fun.args {
            let arg_name = arg.name.raw.clone();
            if self.get_scope().lookup(&arg_name).is_some() {
                return Err(compile_error(
                    self.src.pos_from_offset(arg.name.offset),
                    TypeCkError::AlreadyDefinedVariable(arg_name),
                ));
            }
            let ty = self.get_type_from_ty(&arg.ty).ok_or_else(|| {
                compile_error(
                    self.src.pos_from_offset(arg.ty.offset()),
                    TypeCkError::UndefinedType(arg.ty.to_string()),
                )
            })?;
            // void type can't be used as an arg type
            if ty.is_void() {
                return Err(compile_error(
                    self.src.pos_from_offset(arg.ty.offset()),
                    TypeCkError::InvalidType,
                ));
            }
            let def = VarDef::new(self.gen_def_id(), ty, false);
            self.get_scope_mut().insert_var(arg_name, def);
        }

        for stmt in &fun.block.stmts {
            self.analyze_stmt(&stmt)?;
        }

        let def = FunDef::new(
            self.gen_def_id(),
            ret_ty,
            fun.args
                .iter()
                .map(|arg| {
                    self.get_scope()
                        .lookup(&arg.name.raw)
                        .unwrap()
                        .as_var_def()
                        .ty
                })
                .collect(),
        );
        self.pop_scope();
        self.get_scope_mut().insert_fun(fun.name.raw.clone(), def);
        Ok(())
    }

    fn analyze_stmt(&mut self, stmt: &ast::Stmt) -> Result<()> {
        match &stmt.kind {
            ast::StmtKind::Expr(expr) => {
                self.analyze_expr(&expr)?;
            }
            ast::StmtKind::VarDecl {
                keyword: _,
                name,
                init,
            } => {
                if self.get_scope().lookup(&name.raw).is_some() {
                    return Err(compile_error(
                        self.src.pos_from_offset(name.offset),
                        TypeCkError::AlreadyDefinedVariable(name.raw.clone()),
                    ));
                }
                let init_ty = self.analyze_expr(&init)?;
                let var_ty = self.ty_arena.alloc_var();
                let def_id = self.gen_def_id();
                init_ty.set_prune(var_ty);
                let def = VarDef::new(def_id, init_ty, true);
                self.get_scope_mut().insert_var(name.raw.clone(), def);
            }
            ast::StmtKind::Ret { keyword: _, expr } => {
                let ty = match &expr {
                    Some(expr) => self.analyze_expr(&expr)?,
                    None => self.ty_arena.alloc_void(),
                };
                if let Some(ret_ty) = self.current_fn_ret_ty {
                    ty.set_prune(ret_ty);
                }
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
            ast::ExprKind::Var(tok) => match self.get_scope().lookup(&tok.raw) {
                Some(def) => def.as_var_def().ty,
                None => {
                    return Err(compile_error(
                        self.src.pos_from_offset(tok.offset),
                        TypeCkError::UndefinedVariable(tok.raw.clone()),
                    ));
                }
            },
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

    fn unify_ty(
        &self,
        ty: &'infer InferTy<'infer>,
    ) -> std::result::Result<&'infer InferTy<'infer>, TypeCkError> {
        let mut ty = ty;
        for r_ty in ty.borrow_from_nodes().iter() {
            ty = unify(ty, self.unify_ty(r_ty)?)?;
        }
        Ok(ty)
    }

    pub fn get_final_type(
        &self,
        ty: &'infer InferTy<'infer>,
    ) -> std::result::Result<ty::Type, TypeCkError> {
        let top_ty = ty.prune();
        let final_kind = self.unify_ty(top_ty)?.kind;
        let final_ty = match &final_kind {
            InferTyKind::Var => {
                return Err(TypeCkError::UnresolvedType);
            }
            InferTyKind::Int(int_kind) => match int_kind {
                IntKind::I8 => ty::Type::Int(ty::IntKind::I8),
                IntKind::I16 => ty::Type::Int(ty::IntKind::I16),
                IntKind::I32 => ty::Type::Int(ty::IntKind::I32),
                IntKind::I64 => ty::Type::Int(ty::IntKind::I64),
            },
            InferTyKind::IntLit => ty::Type::Int(ty::IntKind::I64),
            InferTyKind::Float(kind) => match kind {
                FloatKind::F32 => ty::Type::Float(ty::FloatKind::F32),
                FloatKind::F64 => ty::Type::Float(ty::FloatKind::F64),
            },
            InferTyKind::FloatLit => ty::Type::Float(ty::FloatKind::F64),
            InferTyKind::Bool => ty::Type::Bool,
            InferTyKind::Void => ty::Type::Void,
        };
        Ok(final_ty)
    }
}

fn unify<'infer>(
    a: &'infer InferTy<'infer>,
    b: &'infer InferTy<'infer>,
) -> std::result::Result<&'infer InferTy<'infer>, TypeCkError> {
    match (&a.kind, &b.kind) {
        // Var
        (InferTyKind::Var, _) => Ok(b),
        (_, InferTyKind::Var) => unify(b, a),

        // IntLit
        (InferTyKind::IntLit, InferTyKind::Int(_))
        | (InferTyKind::IntLit, InferTyKind::IntLit)
        | (InferTyKind::IntLit, InferTyKind::Float(_))
        | (InferTyKind::IntLit, InferTyKind::FloatLit) => Ok(b),
        (_, InferTyKind::IntLit) => unify(b, a),

        // FloatLit
        (InferTyKind::FloatLit, InferTyKind::FloatLit)
        | (InferTyKind::FloatLit, InferTyKind::Float(_)) => Ok(b),
        (_, InferTyKind::FloatLit) => unify(b, a),

        (a_kind, b_kind) => {
            if a_kind == b_kind {
                Ok(a)
            } else {
                Err(TypeCkError::ConflictTypes(
                    a_kind.to_string(),
                    b_kind.to_string(),
                ))
            }
        }
    }
}
