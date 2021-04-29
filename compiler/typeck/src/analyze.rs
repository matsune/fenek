use crate::infer_ty::*;
use crate::scope::*;
use error::{CompileError, TypeCkError};
use hir::def::*;
use hir::ty;
use hir::Typed;
use lex::token;
use num_traits::Num;
use pos::{Pos, SrcFile};
use std::collections::HashMap;
use std::convert::From;
use std::str::FromStr;

// https://github.com/rust-lang/rust/issues/22639
fn is_parse_int_overflow_error(e: std::num::ParseIntError) -> bool {
    let pos_overflow_err = "2147483648".parse::<i32>().err().unwrap();
    if e == pos_overflow_err {
        return true;
    }
    let neg_overflow_err = "-2147483649".parse::<i32>().err().unwrap();
    e == neg_overflow_err
}

type Result<T> = std::result::Result<T, CompileError>;

fn compile_error(pos: Pos, typeck_err: TypeCkError) -> CompileError {
    CompileError::new(pos, Box::new(typeck_err))
}

fn strip_base_underscore(base: token::IntBase, literal: &str) -> String {
    match base {
        token::IntBase::Binary => literal
            .strip_prefix("0b")
            .or_else(|| literal.strip_prefix("0B"))
            .unwrap_or(literal)
            .replace("_", ""),
        token::IntBase::Octal => literal
            .strip_prefix("0o")
            .or_else(|| literal.strip_prefix("0O"))
            .unwrap_or(literal)
            .replace("_", ""),
        token::IntBase::Decimal => literal.replace("_", ""),
        token::IntBase::Hex => literal
            .strip_prefix("0x")
            .or_else(|| literal.strip_prefix("0X"))
            .unwrap_or(literal)
            .replace("_", ""),
    }
}

fn parse_int_literal<T: Num>(
    base: token::IntBase,
    literal: &str,
) -> std::result::Result<T, T::FromStrRadixErr> {
    T::from_str_radix(&strip_base_underscore(base, &literal), base.into())
}

pub fn lower(src: &SrcFile, fun: ast::Fun) -> Result<hir::Fun> {
    let lower = {
        // lifetime 'infer
        let ty_arena = InferTyArena::default();
        let mut analyzer = TyAnalyzer::new(src, &ty_arena);
        analyzer.analyze_fun(&fun)?;

        // Finalyze InferTy into ty::Type
        let mut node_ty_map = HashMap::with_capacity(analyzer.node_ty_map.len());
        for (node_id, infer_ty) in analyzer.node_ty_map.iter() {
            let final_ty = analyzer.get_final_type(infer_ty).map_err(|err| {
                let offset = ast::visit::visit_fun(&fun, *node_id, |node| node.offset()).unwrap();
                compile_error(src.pos_from_offset(offset), err)
            })?;
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
                            arg_tys.push(analyzer.get_final_type(arg_ty).unwrap());
                        }
                        let ret_ty = analyzer.get_final_type(fun_def.ret_ty).unwrap();
                        FunDef::new(fun_def.id, ret_ty, arg_tys).into()
                    }
                    Def::Var(var_def) => VarDef::new(
                        var_def.id,
                        analyzer.get_final_type(var_def.ty).unwrap(),
                        var_def.is_mut,
                    )
                    .into(),
                };
                _scope.insert(k.to_string(), def);
            }
            scopes.push(_scope);
        }
        Lower::new(&src, node_ty_map, analyzer.node_scope_map, scopes)
    };
    lower.lower_fun(&fun)
}

struct TyAnalyzer<'src, 'infer> {
    src: &'src SrcFile,
    ty_arena: &'infer InferTyArena<'infer>,
    node_ty_map: HashMap<ast::NodeId, &'infer InferTy<'infer>>,
    node_scope_map: HashMap<ast::NodeId, ArenaIdx>,
    scopes: Vec<ScopeTable<&'infer InferTy<'infer>>>,
    scope_idx: ArenaIdx,
    def_id: DefId,
    current_fn_ret_ty: Option<&'infer InferTy<'infer>>,
}

impl<'src, 'infer> TyAnalyzer<'src, 'infer> {
    fn new(src: &'src SrcFile, ty_arena: &'infer InferTyArena<'infer>) -> Self {
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

    fn analyze_fun(&mut self, fun: &ast::Fun) -> Result<()> {
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

    fn get_final_type(
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

type LowerResult<T> = std::result::Result<T, (ast::NodeId, TypeCkError)>;

struct Lower<'src> {
    src: &'src SrcFile,
    ty_map: HashMap<ast::NodeId, ty::Type>,
    node_scope_map: HashMap<ast::NodeId, ArenaIdx>,
    scopes: Vec<ScopeTable<ty::Type>>,
}

impl<'src> Lower<'src> {
    fn new(
        src: &'src SrcFile,
        ty_map: HashMap<ast::NodeId, ty::Type>,
        node_scope_map: HashMap<ast::NodeId, ArenaIdx>,
        scopes: Vec<ScopeTable<ty::Type>>,
    ) -> Self {
        Self {
            src,
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

    fn lower_fun(&self, fun: &ast::Fun) -> Result<hir::Fun> {
        let id = fun.id;
        let scope = self.scopes.get(0).unwrap();
        let fun_def = scope.lookup(&fun.name.raw).unwrap();
        let ret_ty = match &fun.ret_ty {
            Some(ty) => self.ty_map.get(&ty.id).unwrap().clone(),
            None => ty::Type::Void,
        };
        let scope: &ScopeTable<ty::Type> = self
            .scopes
            .get(*self.node_scope_map.get(&fun.block.id).unwrap())
            .unwrap();
        let mut args = Vec::new();
        for arg in &fun.args {
            let def = scope.lookup(&arg.name.raw).unwrap().as_var_def();
            args.push(hir::Ident::new(
                arg.name.raw.clone(),
                VarDef::new(def.id, self.get_type_from_name(&arg.name.raw), def.is_mut).into(),
            ));
        }
        let mut stmts = Vec::new();
        let stmts_len = fun.block.stmts.len();
        for (idx, stmt) in fun.block.stmts.iter().enumerate() {
            let stmt_offset = stmt.offset();
            if matches!(stmt.kind, ast::StmtKind::Empty(_)) {
                continue;
            }
            let stmt = self.lower_stmt(scope, &stmt).map_err(|(id, err)| {
                let offset = ast::visit::visit_fun(&fun, id, |node| node.offset()).unwrap();
                compile_error(self.src.pos_from_offset(offset), err)
            })?;
            let is_last = idx == stmts_len - 1;
            match &stmt {
                hir::Stmt::Ret(ret) if is_last => {
                    let expr_ty = match &ret.expr {
                        Some(expr) => expr.get_type().clone(),
                        None => ty::Type::Void,
                    };
                    if expr_ty != ret_ty {
                        return Err(compile_error(
                            self.src.pos_from_offset(stmt_offset),
                            TypeCkError::InvalidReturnType,
                        ));
                    }
                }
                hir::Stmt::Ret(ret) if !is_last => {
                    // warning? ret statement should not be in the middle of block
                }
                _ if is_last && !ret_ty.is_void() => {
                    // error if fun_ret_ty is not void
                    return Err(compile_error(
                        self.src.pos_from_offset(stmt_offset),
                        TypeCkError::MustBeRetStmt,
                    ));
                }
                _ => {}
            }
            stmts.push(stmt);
        }

        let def = FunDef::new(
            fun_def.id(),
            ret_ty.clone(),
            args.iter().map(|v| v.def.as_var_def().ty.clone()).collect(),
        );
        let name = hir::Ident::new(fun.name.raw.clone(), def.clone().into());
        Ok(hir::Fun::new(
            id,
            name,
            args,
            ret_ty,
            hir::Block::new(fun.block.id, stmts),
            def,
        ))
    }

    fn lower_stmt(&self, scope: &ScopeTable<ty::Type>, stmt: &ast::Stmt) -> LowerResult<hir::Stmt> {
        let id = stmt.id;
        let stmt = match &stmt.kind {
            ast::StmtKind::Expr(expr) => self.lower_expr(scope, &expr)?.into(),
            ast::StmtKind::VarDecl {
                keyword,
                name,
                init,
            } => {
                let expr = self.lower_expr(scope, &init)?;
                let ty = expr.get_type().clone();
                let def = scope.lookup(&name.raw).unwrap().as_var_def();
                hir::VarDecl::new(
                    id,
                    name.clone(),
                    Box::new(expr),
                    VarDef::new(def.id, ty, def.is_mut),
                )
                .into()
            }
            ast::StmtKind::Ret { keyword, expr } => {
                let expr = match expr {
                    Some(expr) => Some(self.lower_expr(scope, &expr)?),
                    None => None,
                };
                hir::Ret::new(id, expr).into()
            }
            ast::StmtKind::Empty(_) => unreachable!(),
        };
        Ok(stmt)
    }

    fn lower_lit(&self, id: ast::NodeId, lit: &ast::Lit) -> LowerResult<hir::Lit> {
        let ty = self.ty_map.get(&id).unwrap();
        let lit_kind = self
            .lower_literal(ty, &lit.kind, &lit.token.raw)
            .map_err(|err| (id, err))?;
        Ok(hir::Lit::new(id, lit_kind, ty.clone()))
    }

    fn lower_literal(
        &self,
        ty: &ty::Type,
        lit_kind: &ast::LitKind,
        literal: &str,
    ) -> std::result::Result<hir::LitKind, TypeCkError> {
        let kind = match lit_kind {
            // int literal
            ast::LitKind::Int(base) => match ty {
                // to int
                ty::Type::Int(kind) => match kind {
                    // to i8
                    ty::IntKind::I8 => {
                        let n = parse_int_literal(*base, &literal).map_err(|err| {
                            if is_parse_int_overflow_error(err) {
                                TypeCkError::OverflowInt(literal.to_string(), "i8".to_string())
                            } else {
                                TypeCkError::InvalidInt(literal.to_string())
                            }
                        })?;
                        hir::LitKind::I8(n)
                    }
                    // to i16
                    ty::IntKind::I16 => {
                        let n = parse_int_literal(*base, &literal).map_err(|err| {
                            if is_parse_int_overflow_error(err) {
                                TypeCkError::OverflowInt(literal.to_string(), "i16".to_string())
                            } else {
                                TypeCkError::InvalidInt(literal.to_string())
                            }
                        })?;
                        hir::LitKind::I16(n)
                    }
                    // to i32
                    ty::IntKind::I32 => {
                        let n = parse_int_literal(*base, &literal).map_err(|err| {
                            if is_parse_int_overflow_error(err) {
                                TypeCkError::OverflowInt(literal.to_string(), "i32".to_string())
                            } else {
                                TypeCkError::InvalidInt(literal.to_string())
                            }
                        })?;
                        hir::LitKind::I32(n)
                    }
                    // to i64
                    ty::IntKind::I64 => {
                        let n = parse_int_literal(*base, &literal).map_err(|err| {
                            if is_parse_int_overflow_error(err) {
                                TypeCkError::OverflowInt(literal.to_string(), "i64".to_string())
                            } else {
                                TypeCkError::InvalidInt(literal.to_string())
                            }
                        })?;
                        hir::LitKind::I64(n)
                    }
                },
                ty::Type::Float(kind) => match kind {
                    // to f32
                    ty::FloatKind::F32 => {
                        let v = f32::from_str(&literal)
                            .map_err(|_| TypeCkError::InvalidFloat(literal.to_string()))?;
                        if v == f32::INFINITY {
                            return Err(TypeCkError::InvalidFloat(literal.to_string()));
                        }
                        hir::LitKind::F32(v)
                    }
                    // to f64
                    ty::FloatKind::F64 => {
                        let v = f64::from_str(&literal)
                            .map_err(|_| TypeCkError::InvalidFloat(literal.to_string()))?;
                        if v == f64::INFINITY {
                            return Err(TypeCkError::InvalidFloat(literal.to_string()));
                        }
                        hir::LitKind::F64(v)
                    }
                },
                _ => return Err(TypeCkError::InvalidType),
            },
            // float literal
            ast::LitKind::Float => match ty {
                ty::Type::Float(float_kind) => match float_kind {
                    // to f32
                    ty::FloatKind::F32 => {
                        let v = f32::from_str(&literal)
                            .map_err(|_| TypeCkError::InvalidFloat(literal.to_string()))?;
                        if v == f32::INFINITY {
                            return Err(TypeCkError::InvalidFloat(literal.to_string()));
                        }
                        hir::LitKind::F32(v)
                    }
                    // to f64
                    ty::FloatKind::F64 => {
                        let v = f64::from_str(&literal)
                            .map_err(|_| TypeCkError::InvalidFloat(literal.to_string()))?;
                        if v == f64::INFINITY {
                            return Err(TypeCkError::InvalidFloat(literal.to_string()));
                        }
                        hir::LitKind::F64(v)
                    }
                },
                _ => return Err(TypeCkError::InvalidType),
            },
            // bool literal
            ast::LitKind::Bool => match ty {
                // to bool
                ty::Type::Bool => hir::LitKind::Bool(literal == "true"),
                // bool can't be other types
                _ => return Err(TypeCkError::InvalidType),
            },
            ast::LitKind::String => unimplemented!(),
        };
        Ok(kind)
    }

    fn lower_expr(&self, scope: &ScopeTable<ty::Type>, expr: &ast::Expr) -> LowerResult<hir::Expr> {
        let id = expr.id;
        let ty = self.ty_map.get(&id).unwrap().clone();
        match &expr.kind {
            ast::ExprKind::Lit(lit) => self.lower_lit(id, lit).map(|v| v.into()),
            ast::ExprKind::Var(tok) => {
                let def = scope.lookup(&tok.raw).unwrap().as_var_def();
                Ok(
                    hir::Ident::new(tok.raw.clone(), VarDef::new(def.id, ty, def.is_mut).into())
                        .into(),
                )
            }
            ast::ExprKind::Binary(op, lhs, rhs) => self.lower_binary(scope, id, op, lhs, rhs),
            ast::ExprKind::Unary(op, expr) => {
                match &expr.kind {
                    ast::ExprKind::Lit(lit) => {
                        match op.op_kind() {
                            // ast::UnaryOpKind::Add => {
                            //     // + should have number
                            //     if !ty.is_int() && !ty.is_float() {
                            //         return Err(compile_error(
                            //             lit.pos,
                            //             TypeCkError::InvalidUnaryTypes,
                            //         ));
                            //     }
                            //     self.lower_lit(unary.expr.id, &lit).map(|v| v.into())
                            // }
                            ast::UnaryOpKind::Minus => {
                                // - should have number
                                if !ty.is_int() && !ty.is_float() {
                                    return Err((id, TypeCkError::InvalidUnaryTypes));
                                }
                                let mut literal = String::from("-");
                                literal.push_str(&lit.token.raw);
                                self.lower_literal(&ty, &lit.kind, &literal)
                                    .map(|lit_kind| hir::Lit::new(id, lit_kind, ty).into())
                                    .map_err(|err| (id, err))
                            }
                            ast::UnaryOpKind::Not => {
                                // ! should have bool
                                if !ty.is_bool() {
                                    return Err((id, TypeCkError::InvalidUnaryTypes));
                                }
                                self.lower_lit(id, &lit).map(|v| {
                                    // toggle bool value
                                    let mut v = v;
                                    v.kind = hir::LitKind::Bool(!v.kind.as_bool());
                                    v.into()
                                })
                            }
                        }
                    }
                    _ => {
                        let expr = self.lower_expr(scope, &expr)?;
                        let expr_ty = expr.get_type();
                        match op.op_kind() {
                            ast::UnaryOpKind::Minus => {
                                // + and - should have number
                                if !expr_ty.is_int() && !expr_ty.is_float() {
                                    return Err((id, TypeCkError::InvalidUnaryTypes));
                                }
                            }
                            ast::UnaryOpKind::Not => {
                                // ! should have bool
                                if !expr_ty.is_bool() {
                                    return Err((id, TypeCkError::InvalidUnaryTypes));
                                }
                            }
                        }
                        Ok(hir::Unary::new(id, op.op_kind(), expr).into())
                    }
                }
            }
        }
    }

    fn lower_binary(
        &self,
        scope: &ScopeTable<ty::Type>,
        id: ast::NodeId,
        op: &ast::BinOp,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> LowerResult<hir::Expr> {
        let lhs = self.lower_expr(scope, lhs)?;
        let rhs = self.lower_expr(scope, rhs)?;
        match op.op_kind() {
            ast::BinOpKind::Add
            | ast::BinOpKind::Sub
            | ast::BinOpKind::Mul
            | ast::BinOpKind::Div => match (lhs.get_type(), rhs.get_type()) {
                (ty::Type::Int(lty), ty::Type::Int(rty)) if lty == rty => {}
                (ty::Type::Float(lty), ty::Type::Float(rty)) if lty == rty => {}
                _ => return Err((id, TypeCkError::InvalidBinaryTypes)),
            },
        };
        Ok(hir::Binary::new(
            id,
            op.op_kind(),
            lhs,
            rhs,
            self.ty_map.get(&id).unwrap().clone(),
        )
        .into())
    }
}
