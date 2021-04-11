use crate::infer_ty::*;
use crate::mir;
use crate::mir::Typed;
use crate::scope::*;
use crate::ty;
use error::{CompileError, Pos, TypeCkError};
use parse::ast;
use parse::ast::Node;
use std::collections::HashMap;

#[cfg(test)]
mod tests;

type Result<T> = std::result::Result<T, CompileError>;

fn compile_error(pos: Pos, typeck_err: TypeCkError) -> CompileError {
    CompileError::new(pos, Box::new(typeck_err))
}

pub fn lower(fun: &ast::Fun) -> Result<mir::Fun> {
    let lower = {
        // infer types and return Lower instance
        let ty_arena = InferTyArena::default();
        let mut analyzer = TyAnalyzer::new(&ty_arena);
        analyzer.analyze_fun(&fun)?;

        // Finalyze InferTy into ty::Type
        let mut node_ty_map = HashMap::with_capacity(analyzer.node_ty_map.len());
        for (node_id, infer_ty) in analyzer.node_ty_map.iter() {
            let final_ty = analyzer.get_final_type(infer_ty)?;
            node_ty_map.insert(*node_id, final_ty);
        }

        let mut scopes = Vec::with_capacity(analyzer.scopes.len());
        for scope in analyzer.scopes.iter() {
            let mut _scope = ScopeTable::new(scope.idx, scope.parent);
            for (k, v) in scope.table.iter() {
                let def = match v {
                    Def::Fun(fun_def) => {
                        let mut arg_tys = Vec::new();
                        for arg_ty in fun_def.arg_tys.iter() {
                            arg_tys.push(analyzer.get_final_type(arg_ty)?);
                        }
                        let ret_ty = analyzer.get_final_type(fun_def.ret_ty)?;
                        FunDef::new(fun_def.id, ret_ty, arg_tys).into()
                    }
                    Def::Var(var_def) => VarDef::new(
                        var_def.id,
                        analyzer.get_final_type(var_def.ty)?,
                        var_def.is_mut,
                    )
                    .into(),
                };
                _scope.insert(k.to_string(), def);
            }
            scopes.push(_scope);
        }
        Lower::new(node_ty_map, analyzer.node_scope_map, scopes)
    };
    lower.lower_fun(&fun)
}

struct TyAnalyzer<'a> {
    ty_arena: &'a InferTyArena<'a>,
    node_ty_map: HashMap<ast::NodeId, &'a InferTy<'a>>,
    node_scope_map: HashMap<ast::NodeId, ArenaIdx>,
    scopes: Vec<ScopeTable<&'a InferTy<'a>>>,
    scope_idx: ArenaIdx,
    def_id: DefId,
    current_fn_ret_ty: Option<&'a InferTy<'a>>,
}

impl<'a> TyAnalyzer<'a> {
    fn new(ty_arena: &'a InferTyArena<'a>) -> Self {
        let mut scopes = Vec::new();
        let scope_idx = scopes.len();
        // global scope
        scopes.push(ScopeTable::new(scope_idx, None));
        TyAnalyzer {
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

    fn get_scope(&self) -> &ScopeTable<&'a InferTy<'a>> {
        &self.scopes[self.scope_idx]
    }

    fn get_scope_mut(&mut self) -> &mut ScopeTable<&'a InferTy<'a>> {
        &mut self.scopes[self.scope_idx]
    }

    fn gen_def_id(&mut self) -> DefId {
        let id = self.def_id;
        self.def_id += 1;
        id
    }

    fn analyze_fun(&mut self, fun: &ast::Fun) -> Result<()> {
        let ret_ty = match &fun.ret_ty {
            Some(ret_ty) => {
                let ty = self.get_type_from_name(&ret_ty.raw).ok_or_else(|| {
                    compile_error(ret_ty.pos, TypeCkError::UndefinedType(ret_ty.raw.clone()))
                })?;
                self.node_ty_map.insert(ret_ty.id(), ty);
                ty
            }
            None => self.ty_arena.alloc_void(),
        };
        self.current_fn_ret_ty = Some(ret_ty);
        self.push_scope();
        self.node_scope_map.insert(fun.block.id(), self.scope_idx);

        for arg in &fun.args {
            if self.get_scope().lookup(&arg.name.raw).is_some() {
                return Err(compile_error(
                    arg.name.pos,
                    TypeCkError::AlreadyDefinedVariable(arg.name.raw.clone()),
                ));
            }
            let ty = self.get_type_from_name(&arg.ty.raw).ok_or_else(|| {
                compile_error(arg.ty.pos, TypeCkError::UndefinedType(arg.ty.raw.clone()))
            })?;
            // void type can't be used as an arg type
            if ty.is_void() {
                return Err(compile_error(arg.ty.pos, TypeCkError::InvalidType));
            }
            let def = VarDef::new(self.gen_def_id(), ty, false);
            self.get_scope_mut().insert_var(arg.name.raw.clone(), def);
        }

        for stmt in &fun.block.stmts {
            self.analyze_stmt(&stmt)?;
        }
        // if !ret_ty.get().is_void() {
        //     // last statement must be ret
        //     let last_stmt = stmts
        //         .last()
        //         .ok_or_else(|| compile_error(fun.pos, TypeCkError::InvalidReturnType))?;
        //     if !matches!(last_stmt, mir::Stmt::Ret(_)) {
        //         return Err(compile_error(
        //             fun.block.stmts.last().unwrap().pos(),
        //             TypeCkError::InvalidReturnType,
        //         ));
        //     }
        // }

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

    fn get_type_from_name(&self, ty_name: &str) -> Option<&'a InferTy<'a>> {
        let ty = match ty_name {
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
        };
        Some(ty)
    }

    fn analyze_stmt(&mut self, stmt: &ast::Stmt) -> Result<()> {
        match stmt {
            ast::Stmt::Expr(expr) => {
                self.analyze_expr(expr)?;
            }
            ast::Stmt::VarDecl(var_decl) => {
                if self.get_scope().lookup(&var_decl.name.raw).is_some() {
                    return Err(compile_error(
                        var_decl.name.pos,
                        TypeCkError::AlreadyDefinedVariable(var_decl.name.raw.clone()),
                    ));
                }
                let expr_ty = self.analyze_expr(&var_decl.init)?;
                let var_ty = self.ty_arena.alloc_var();
                let def_id = self.gen_def_id();
                expr_ty.set_prune(var_ty);
                let def = VarDef::new(def_id, expr_ty, true);
                self.get_scope_mut()
                    .insert_var(var_decl.name.raw.clone(), def);
            }
            ast::Stmt::Ret(ret) => {
                let ty = match &ret.expr {
                    Some(expr) => self.analyze_expr(&expr)?,
                    None => self.ty_arena.alloc_void(),
                };
                if let Some(ret_ty) = self.current_fn_ret_ty {
                    ty.set_prune(ret_ty);
                }
            }
        }
        Ok(())
    }

    fn analyze_expr(&mut self, expr: &ast::Expr) -> Result<&'a InferTy<'a>> {
        let id = expr.id();
        let ty = match expr {
            ast::Expr::Lit(lit) => match lit.kind {
                ast::LitKind::Int(_) => self.ty_arena.alloc_int_lit(),
                ast::LitKind::Float(_) => self.ty_arena.alloc_float_lit(),
                ast::LitKind::Bool(_) => self.ty_arena.alloc_bool(),
                ast::LitKind::String(_) => unimplemented!(),
            },
            ast::Expr::Ident(ident) => match self.get_scope().lookup(&ident.raw) {
                Some(def) => def.as_var_def().ty,
                None => {
                    return Err(compile_error(
                        ident.pos,
                        TypeCkError::UndefinedVariable(ident.raw.clone()),
                    ))
                }
            },
            ast::Expr::Binary(binary) => self.analyze_binary(binary)?,
            ast::Expr::Unary(unary) => self.analyze_unary(unary)?,
        };
        self.node_ty_map.insert(id, ty);
        Ok(ty)
    }

    fn analyze_binary(&mut self, binary: &ast::Binary) -> Result<&'a InferTy<'a>> {
        let binary_ty = self.ty_arena.alloc_var();
        self.node_ty_map.insert(binary.id(), binary_ty);
        let lhs_ty = self.analyze_expr(&binary.lhs)?;
        let rhs_ty = self.analyze_expr(&binary.rhs)?;
        lhs_ty.set_prune(binary_ty);
        rhs_ty.set_prune(binary_ty);
        Ok(binary_ty)
    }

    fn analyze_unary(&mut self, unary: &ast::Unary) -> Result<&'a InferTy<'a>> {
        let unary_ty = self.ty_arena.alloc_var();
        self.node_ty_map.insert(unary.id(), unary_ty);
        let expr_ty = self.analyze_expr(&unary.expr)?;
        expr_ty.set_prune(unary_ty);
        Ok(unary_ty)
    }

    fn unify_ty(&self, ty: &'a InferTy<'a>) -> Result<&'a InferTy<'a>> {
        let mut ty = ty;
        for r_ty in ty.borrow_from_nodes().iter() {
            ty = unify(ty, self.unify_ty(r_ty)?)?;
        }
        Ok(ty)
    }

    fn get_final_type(&self, ty: &'a InferTy<'a>) -> Result<ty::Type> {
        let top_ty = ty.prune();
        let final_kind = self.unify_ty(top_ty)?.kind;
        let final_ty = match &final_kind {
            InferTyKind::Var => {
                // FIXME: pos
                return Err(compile_error(Pos::default(), TypeCkError::UnresolvedType));
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

fn unify<'a>(a: &'a InferTy<'a>, b: &'a InferTy<'a>) -> Result<&'a InferTy<'a>> {
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
                Err(CompileError::new(
                    // FIXME: pos
                    Pos::default(),
                    Box::new(TypeCkError::ConflictTypes(
                        a_kind.to_string(),
                        b_kind.to_string(),
                    )),
                ))
            }
        }
    }
}

struct Lower {
    ty_map: HashMap<ast::NodeId, ty::Type>,
    node_scope_map: HashMap<ast::NodeId, ArenaIdx>,
    scopes: Vec<ScopeTable<ty::Type>>,
}

impl Lower {
    fn new(
        ty_map: HashMap<ast::NodeId, ty::Type>,
        node_scope_map: HashMap<ast::NodeId, ArenaIdx>,
        scopes: Vec<ScopeTable<ty::Type>>,
    ) -> Self {
        Self {
            ty_map,
            scopes,
            node_scope_map,
        }
    }

    fn get_type_from_name(&self, ty_name: &str) -> ty::Type {
        match ty_name {
            "i8" => ty::Type::Int(ty::IntKind::I8),
            "i16" => ty::Type::Int(ty::IntKind::I16),
            "i32" => ty::Type::Int(ty::IntKind::I32),
            "i64" => ty::Type::Int(ty::IntKind::I64),
            "f32" => ty::Type::Float(ty::FloatKind::F32),
            "f64" => ty::Type::Float(ty::FloatKind::F64),
            "bool" => ty::Type::Bool,
            // "string" => self.ty_arena.alloc_string(),
            "void" => ty::Type::Void,
            _ => unreachable!(),
        }
    }

    fn lower_fun(&self, fun: &ast::Fun) -> Result<mir::Fun> {
        let scope = self.scopes.get(0).unwrap();
        let fun_def = scope.lookup(&fun.name.raw).unwrap();
        let ret_ty = match &fun.ret_ty {
            Some(ty) => self.ty_map.get(&ty.id()).unwrap().clone(),
            None => ty::Type::Void,
        };
        let scope: &ScopeTable<ty::Type> = self
            .scopes
            .get(*self.node_scope_map.get(&fun.block.id()).unwrap())
            .unwrap();
        let mut args = Vec::new();
        for arg in &fun.args {
            let def = scope.lookup(&arg.name.raw).unwrap().as_var_def();
            args.push(mir::Ident::new(
                arg.name.raw.clone(),
                VarDef::new(def.id, self.get_type_from_name(&arg.name.raw), def.is_mut).into(),
            ));
        }
        let mut stmts = Vec::new();
        for stmt in &fun.block.stmts {
            stmts.push(self.lower_stmt(scope, &stmt)?);
        }
        let def = FunDef::new(
            fun_def.id(),
            ret_ty.clone(),
            args.iter().map(|v| v.def.as_var_def().ty.clone()).collect(),
        );
        let name = mir::Ident::new(fun.name.raw.clone(), def.clone().into());
        Ok(mir::Fun::new(
            fun.id,
            name,
            args,
            ret_ty,
            mir::Block::new(fun.block.id, stmts),
            def,
        ))
    }

    fn lower_stmt(&self, scope: &ScopeTable<ty::Type>, stmt: &ast::Stmt) -> Result<mir::Stmt> {
        let stmt = match stmt {
            ast::Stmt::Expr(expr) => self.lower_expr(scope, expr)?.into(),
            ast::Stmt::VarDecl(var_decl) => {
                let expr = self.lower_expr(scope, &var_decl.init)?;
                let ty = expr.get_type().clone();
                let def = scope.lookup(&var_decl.name.raw).unwrap().as_var_def();
                mir::VarDecl::new(
                    var_decl.id,
                    var_decl.name.clone(),
                    Box::new(expr),
                    VarDef::new(def.id, ty, def.is_mut),
                )
                .into()
            }
            ast::Stmt::Ret(ret) => {
                let expr = match &ret.expr {
                    Some(expr) => Some(self.lower_expr(scope, &expr)?),
                    None => None,
                };
                mir::Ret::new(ret.id, expr).into()
            }
        };
        Ok(stmt)
    }

    fn lower_expr(&self, scope: &ScopeTable<ty::Type>, expr: &ast::Expr) -> Result<mir::Expr> {
        let ty = self.ty_map.get(&expr.id()).unwrap().clone();
        match expr {
            ast::Expr::Lit(lit) => Ok(mir::Lit::new(lit.id, lit.kind.clone(), ty).into()),
            ast::Expr::Ident(ident) => {
                let def = scope.lookup(&ident.raw).unwrap().as_var_def();
                Ok(mir::Ident::new(
                    ident.raw.clone(),
                    VarDef::new(def.id, ty, def.is_mut).into(),
                )
                .into())
            }
            ast::Expr::Binary(binary) => self.lower_binary(scope, binary),
            ast::Expr::Unary(unary) => self.lower_unary(scope, unary),
        }
    }

    fn lower_binary(
        &self,
        scope: &ScopeTable<ty::Type>,
        binary: &ast::Binary,
    ) -> Result<mir::Expr> {
        let lhs = self.lower_expr(scope, &binary.lhs)?;
        let rhs = self.lower_expr(scope, &binary.rhs)?;
        match binary.op.symbol.as_str() {
            "+" | "-" | "*" | "/" => match (lhs.get_type(), rhs.get_type()) {
                (ty::Type::Int(lty), ty::Type::Int(rty)) if lty == rty => {}
                (ty::Type::Float(lty), ty::Type::Float(rty)) if lty == rty => {}
                _ => {
                    return Err(compile_error(
                        binary.lhs.pos(),
                        TypeCkError::InvalidBinaryTypes,
                    ))
                }
            },
            _ => unimplemented!(),
        };
        Ok(mir::Binary::new(
            binary.id,
            binary.op.clone(),
            lhs,
            rhs,
            self.ty_map.get(&binary.id).unwrap().clone(),
        )
        .into())
    }

    fn lower_unary(&self, scope: &ScopeTable<ty::Type>, unary: &ast::Unary) -> Result<mir::Expr> {
        let expr = self.lower_expr(scope, &unary.expr)?;
        let ty = expr.get_type();
        match unary.op {
            ast::UnaryOp::Add | ast::UnaryOp::Sub => {
                if !ty.is_int() && !ty.is_float() {
                    return Err(compile_error(
                        unary.expr.pos(),
                        TypeCkError::InvalidUnaryTypes,
                    ));
                }
            }
            ast::UnaryOp::Not => {
                if !ty.is_bool() {
                    return Err(compile_error(
                        unary.expr.pos(),
                        TypeCkError::InvalidUnaryTypes,
                    ));
                }
            }
        };
        Ok(mir::Unary::new(unary.id, unary.op, expr).into())
    }
}
