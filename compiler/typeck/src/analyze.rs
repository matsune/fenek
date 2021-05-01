use crate::infer_ty::*;
use crate::scope::*;
use error::{CompileError, Result, TypeCkError};
use hir::def::*;
use hir::ty;
use pos::{Pos, SrcFile};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

fn compile_error(pos: Pos, typeck_err: TypeCkError) -> CompileError {
    CompileError::new(pos, Box::new(typeck_err))
}

pub struct TyAnalyzer<'src, 'infer> {
    src: &'src SrcFile,
    ty_arena: &'infer InferTyArena<'infer>,
    pub node_ty_map: HashMap<ast::NodeId, &'infer InferTy<'infer>>,
    pub node_def_map: HashMap<ast::NodeId, Rc<Def<&'infer InferTy<'infer>>>>,
    scopes: Vec<ScopeTable<&'infer InferTy<'infer>>>,
    scope_idx: ArenaIdx,
    def_id: DefId,
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
            node_ty_map: HashMap::new(),
            node_def_map: HashMap::new(),
            scopes,
            scope_idx,
            def_id: 0,
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

    fn lookup_fun(&self, name: &str) -> Option<Rc<Def<&'infer InferTy<'infer>>>> {
        let scopes = &self.scopes;
        let mut tmp_idx = self.scope_idx;
        while let Some(scope_table) = scopes.get(tmp_idx) {
            match scope_table.lookup_fun(name) {
                Some(fun_def) => {
                    return Some(fun_def.clone());
                }
                None => match scope_table.parent {
                    Some(parent_idx) => tmp_idx = parent_idx,
                    None => break,
                },
            }
        }
        None
    }

    pub fn analyze_module(&mut self, module: &ast::Module) -> Result<()> {
        for fun in &module.funs {
            if self.lookup_fun(&fun.name.raw).is_some() {
                return Err(compile_error(
                    self.src.pos_from_offset(fun.name.offset),
                    TypeCkError::AlreadyDefinedFun(fun.name.raw.clone()),
                ));
            }
            let fun_def = self.make_fun_def(&fun)?;
            let rc = self
                .get_scope_mut()
                .insert_fun(fun.name.raw.clone(), fun_def);
            self.node_def_map.insert(fun.id, rc);
        }

        for fun in &module.funs {
            self.analyze_fun(&fun)?;
        }
        Ok(())
    }

    fn make_fun_def(&mut self, fun: &ast::Fun) -> Result<Def<&'infer InferTy<'infer>>> {
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

        Ok(Def::new(
            self.gen_def_id(),
            self.ty_arena.alloc_fun(arg_tys, ret_ty),
            false,
        ))
    }

    fn analyze_fun(&mut self, fun: &ast::Fun) -> Result<()> {
        let fun_def = self.lookup_fun(&fun.name.raw).unwrap();
        self.push_scope();
        // self.node_scope_map.insert(fun.block.id, self.scope_idx);

        for (idx, arg) in fun.args.iter().enumerate() {
            let ty = fun_def.ty.kind.as_fun().arg_tys[idx];
            let def = Def::new(self.gen_def_id(), ty, false);
            let def = self.get_scope_mut().insert_var(arg.name.raw.clone(), def);
            self.node_def_map.insert(arg.id, def);
        }

        for stmt in &fun.block.stmts {
            self.analyze_stmt(&stmt, &fun_def.ty.kind.as_fun().ret_ty)?;
        }

        self.pop_scope();
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
                if self.get_scope().lookup_var(&name.raw).is_some() {
                    return Err(compile_error(
                        self.src.pos_from_offset(name.offset),
                        TypeCkError::AlreadyDefinedVariable(name.raw.clone()),
                    ));
                }
                let init_ty = self.analyze_expr(&init)?;
                let var_ty = self.ty_arena.alloc_var();
                let def_id = self.gen_def_id();
                init_ty.set_prune(var_ty);
                let def = Def::new(def_id, init_ty, true);
                let def = self.get_scope_mut().insert_var(name.raw.clone(), def);
                self.node_def_map.insert(stmt.id, def);
            }
            ast::StmtKind::Ret { keyword: _, expr } => {
                let ty = match &expr {
                    Some(expr) => self.analyze_expr(&expr)?,
                    None => self.ty_arena.alloc_void(),
                };
                ty.set_prune(current_fn_ret_ty);
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
            ast::ExprKind::Path(tok) => match self.get_scope().lookup_var(&tok.raw) {
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
                let def = self.lookup_fun(&path.raw).ok_or_else(|| {
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
                self.node_def_map.insert(expr.id, def.clone());
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

    fn unify_ty(
        &self,
        ty: &'infer InferTy<'infer>,
    ) -> Result<&'infer InferTy<'infer>, TypeCkError> {
        let mut ty = ty;
        for r_ty in ty.borrow_from_nodes().iter() {
            ty = unify(ty, self.unify_ty(r_ty)?)?;
        }
        Ok(ty)
    }

    pub fn get_final_type(&self, ty: &'infer InferTy<'infer>) -> Result<ty::Type, TypeCkError> {
        let top_ty = ty.prune();
        let final_kind = &self.unify_ty(top_ty)?.kind;
        let final_ty = match final_kind {
            InferTyKind::Var => {
                return Err(TypeCkError::UnresolvedType);
            }
            InferTyKind::Int(int_kind) => match int_kind {
                IntKind::I8 => ty::Type::Int(ty::IntType::I8),
                IntKind::I16 => ty::Type::Int(ty::IntType::I16),
                IntKind::I32 => ty::Type::Int(ty::IntType::I32),
                IntKind::I64 => ty::Type::Int(ty::IntType::I64),
            },
            InferTyKind::IntLit => ty::Type::Int(ty::IntType::I64),
            InferTyKind::Float(kind) => match kind {
                FloatKind::F32 => ty::Type::Float(ty::FloatType::F32),
                FloatKind::F64 => ty::Type::Float(ty::FloatType::F64),
            },
            InferTyKind::FloatLit => ty::Type::Float(ty::FloatType::F64),
            InferTyKind::Bool => ty::Type::Bool,
            InferTyKind::Void => ty::Type::Void,
            InferTyKind::Fun(fun_ty) => {
                let mut arg_tys = Vec::new();
                for arg_ty in &fun_ty.arg_tys {
                    arg_tys.push(self.get_final_type(arg_ty)?);
                }
                let ret_ty = self.get_final_type(&fun_ty.ret_ty)?;
                ty::Type::Fun(ty::FunType::new(arg_tys, Box::new(ret_ty)))
            }
        };
        Ok(final_ty)
    }
}

fn unify<'infer>(
    a: &'infer InferTy<'infer>,
    b: &'infer InferTy<'infer>,
) -> Result<&'infer InferTy<'infer>, TypeCkError> {
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
