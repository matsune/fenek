use crate::hir;
use crate::hir::Typed;
use crate::infer_ty::*;
use crate::scope::*;
use crate::ty;
use error::{CompileError, Pos, TypeCkError};
use num_traits::Num;
use parse::ast;
use parse::IntBase;
use std::collections::HashMap;
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

#[cfg(test)]
mod tests;

type Result<T> = std::result::Result<T, CompileError>;

fn compile_error(pos: Pos, typeck_err: TypeCkError) -> CompileError {
    CompileError::new(pos, Box::new(typeck_err))
}

fn parse_int_literal<T: Num>(
    base: IntBase,
    literal: &str,
) -> std::result::Result<T, T::FromStrRadixErr> {
    match base {
        IntBase::Binary => T::from_str_radix(
            &literal.replace("_", "").replace("0b", "").replace("0B", ""),
            2,
        ),
        IntBase::Octal => T::from_str_radix(
            &literal.replace("_", "").replace("0o", "").replace("0O", ""),
            8,
        ),
        IntBase::Decimal => T::from_str_radix(&literal.replace("_", ""), 10),
        IntBase::Hex => T::from_str_radix(
            &literal.replace("_", "").replace("0x", "").replace("0X", ""),
            16,
        ),
    }
}

pub fn lower<'ctx>(fun: &'ctx ast::AstNode<'ctx>) -> Result<hir::Fun> {
    let lower = {
        // infer types and return Lower instance
        let ty_arena = InferTyArena::default();
        let mut analyzer = TyAnalyzer::new(&ty_arena);
        analyzer.analyze(&fun)?;

        // Finalyze InferTy into ty::Type
        let mut node_ty_map = HashMap::with_capacity(analyzer.node_ty_map.len());
        for (node_id, infer_ty) in analyzer.node_ty_map.iter() {
            let final_ty = analyzer
                .get_final_type(infer_ty)
                .map_err(|err| compile_error(*analyzer.node_pos_map.get(node_id).unwrap(), err))?;
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
        Lower::new(node_ty_map, analyzer.node_scope_map, scopes)
    };
    lower.lower_fun(fun.id, fun.kind.as_fun())
}

struct TyAnalyzer<'a> {
    ty_arena: &'a InferTyArena<'a>,
    node_pos_map: HashMap<ast::NodeID, Pos>,
    node_ty_map: HashMap<ast::NodeID, &'a InferTy<'a>>,
    node_scope_map: HashMap<ast::NodeID, ArenaIdx>,
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
            node_pos_map: HashMap::new(),
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

    fn analyze(&mut self, node: &ast::AstNode) -> Result<()> {
        match &node.kind {
            ast::AstKind::Fun(fun) => self.analyze_fun(&fun),
            ast::AstKind::Stmt(stmt) => self.analyze_stmt(node.id, &stmt),
            ast::AstKind::Block(block) => {
                for stmt in &block.stmts {
                    self.analyze(&stmt)?;
                }
                Ok(())
            }
            _ => unimplemented!(),
        }
    }

    fn analyze_fun(&mut self, fun: &ast::Fun) -> Result<()> {
        let ret_ty = match &fun.ret_ty {
            Some(ret_ty) => {
                let ret_ty_id = ret_ty.id;
                let ret_ty = ret_ty.kind.as_ident();
                let ty = self.get_type_from_name(&ret_ty.raw).ok_or_else(|| {
                    compile_error(ret_ty.pos, TypeCkError::UndefinedType(ret_ty.raw.clone()))
                })?;
                self.node_ty_map.insert(ret_ty_id, ty);
                self.node_pos_map.insert(ret_ty_id, ret_ty.pos);
                ty
            }
            None => self.ty_arena.alloc_void(),
        };
        self.current_fn_ret_ty = Some(ret_ty);
        self.push_scope();
        self.node_scope_map.insert(fun.block.id, self.scope_idx);

        for arg in &fun.args {
            let arg_name = arg.name.kind.as_ident();
            if self.get_scope().lookup(&arg_name.raw).is_some() {
                return Err(compile_error(
                    arg_name.pos,
                    TypeCkError::AlreadyDefinedVariable(arg_name.raw.clone()),
                ));
            }
            let arg_ty = arg.ty.kind.as_ident();
            let ty = self.get_type_from_name(&arg_ty.raw).ok_or_else(|| {
                compile_error(arg_ty.pos, TypeCkError::UndefinedType(arg_ty.raw.clone()))
            })?;
            // void type can't be used as an arg type
            if ty.is_void() {
                return Err(compile_error(arg_ty.pos, TypeCkError::InvalidType));
            }
            let def = VarDef::new(self.gen_def_id(), ty, false);
            self.get_scope_mut().insert_var(arg_name.raw.clone(), def);
        }

        self.analyze(fun.block)?;

        let def = FunDef::new(
            self.gen_def_id(),
            ret_ty,
            fun.args
                .iter()
                .map(|arg| {
                    self.get_scope()
                        .lookup(&arg.name.kind.as_ident().raw)
                        .unwrap()
                        .as_var_def()
                        .ty
                })
                .collect(),
        );
        self.pop_scope();
        self.get_scope_mut()
            .insert_fun(fun.name.kind.as_ident().raw.clone(), def);
        Ok(())
    }

    fn analyze_stmt(&mut self, id: ast::NodeID, stmt: &ast::Stmt) -> Result<()> {
        match stmt {
            ast::Stmt::Expr(expr) => {
                self.analyze_expr(id, expr)?;
            }
            ast::Stmt::VarDecl(var_decl) => {
                if self
                    .get_scope()
                    .lookup(&var_decl.name.kind.as_ident().raw)
                    .is_some()
                {
                    return Err(compile_error(
                        var_decl.name.kind.as_ident().pos,
                        TypeCkError::AlreadyDefinedVariable(
                            var_decl.name.kind.as_ident().raw.clone(),
                        ),
                    ));
                }
                let expr_ty = self.analyze_expr(var_decl.expr.id, &var_decl.expr.kind.as_expr())?;
                let var_ty = self.ty_arena.alloc_var();
                let def_id = self.gen_def_id();
                expr_ty.set_prune(var_ty);
                let def = VarDef::new(def_id, expr_ty, true);
                self.get_scope_mut()
                    .insert_var(var_decl.name.kind.as_ident().raw.clone(), def);
            }
            ast::Stmt::Ret(ret) => {
                let ty = match &ret.expr {
                    Some(expr) => self.analyze_expr(expr.id, &expr.kind.as_expr())?,
                    None => self.ty_arena.alloc_void(),
                };
                if let Some(ret_ty) = self.current_fn_ret_ty {
                    ty.set_prune(ret_ty);
                }
            }
        }
        Ok(())
    }

    fn analyze_expr(&mut self, id: ast::NodeID, expr: &ast::Expr) -> Result<&'a InferTy<'a>> {
        let ty = match expr {
            ast::Expr::Lit(lit) => match lit.kind {
                ast::LitKind::Int(_) => self.ty_arena.alloc_int_lit(),
                ast::LitKind::Float => self.ty_arena.alloc_float_lit(),
                ast::LitKind::Bool => self.ty_arena.alloc_bool(),
                ast::LitKind::String => unimplemented!(),
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
            ast::Expr::Binary(binary) => self.analyze_binary(id, binary)?,
            ast::Expr::Unary(unary) => self.analyze_unary(id, unary)?,
        };
        self.node_ty_map.insert(id, ty);
        self.node_pos_map.insert(id, expr.pos());
        Ok(ty)
    }

    fn analyze_binary(&mut self, id: ast::NodeID, binary: &ast::Binary) -> Result<&'a InferTy<'a>> {
        let binary_ty = self.ty_arena.alloc_var();
        self.node_ty_map.insert(id, binary_ty);
        self.node_pos_map.insert(id, binary.lhs.pos());
        let lhs_ty = self.analyze_expr(binary.lhs.id, &binary.lhs.kind.as_expr())?;
        let rhs_ty = self.analyze_expr(binary.rhs.id, &binary.rhs.kind.as_expr())?;
        lhs_ty.set_prune(binary_ty);
        rhs_ty.set_prune(binary_ty);
        Ok(binary_ty)
    }

    fn analyze_unary(&mut self, id: ast::NodeID, unary: &ast::Unary) -> Result<&'a InferTy<'a>> {
        let unary_ty = self.ty_arena.alloc_var();
        self.node_ty_map.insert(id, unary_ty);
        self.node_pos_map.insert(id, unary.op.pos());
        let expr_ty = self.analyze_expr(unary.expr.id, &unary.expr.kind.as_expr())?;
        expr_ty.set_prune(unary_ty);
        Ok(unary_ty)
    }

    fn unify_ty(&self, ty: &'a InferTy<'a>) -> std::result::Result<&'a InferTy<'a>, TypeCkError> {
        let mut ty = ty;
        for r_ty in ty.borrow_from_nodes().iter() {
            ty = unify(ty, self.unify_ty(r_ty)?)?;
        }
        Ok(ty)
    }

    fn get_final_type(&self, ty: &'a InferTy<'a>) -> std::result::Result<ty::Type, TypeCkError> {
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

fn unify<'a>(
    a: &'a InferTy<'a>,
    b: &'a InferTy<'a>,
) -> std::result::Result<&'a InferTy<'a>, TypeCkError> {
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

struct Lower {
    ty_map: HashMap<ast::NodeID, ty::Type>,
    node_scope_map: HashMap<ast::NodeID, ArenaIdx>,
    scopes: Vec<ScopeTable<ty::Type>>,
}

impl Lower {
    fn new(
        ty_map: HashMap<ast::NodeID, ty::Type>,
        node_scope_map: HashMap<ast::NodeID, ArenaIdx>,
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

    fn lower_fun(&self, id: ast::NodeID, fun: &ast::Fun) -> Result<hir::Fun> {
        let scope = self.scopes.get(0).unwrap();
        let fun_def = scope.lookup(&fun.name.kind.as_ident().raw).unwrap();
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
            let def = scope
                .lookup(&arg.name.kind.as_ident().raw)
                .unwrap()
                .as_var_def();
            args.push(hir::Ident::new(
                arg.name.kind.as_ident().raw.clone(),
                VarDef::new(
                    def.id,
                    self.get_type_from_name(&arg.name.kind.as_ident().raw),
                    def.is_mut,
                )
                .into(),
            ));
        }
        let mut stmts = Vec::new();
        let block = fun.block.kind.as_block();
        for (idx, stmt) in block.stmts.iter().enumerate() {
            let pos = stmt.pos();
            let stmt = self.lower_stmt(scope, stmt.id, &stmt.kind.as_stmt())?;
            let is_last = idx == block.stmts.len() - 1;
            match &stmt {
                hir::Stmt::Ret(ret) if is_last => {
                    let expr_ty = match &ret.expr {
                        Some(expr) => expr.get_type().clone(),
                        None => ty::Type::Void,
                    };
                    if expr_ty != ret_ty {
                        return Err(compile_error(pos, TypeCkError::InvalidReturnType));
                    }
                }
                hir::Stmt::Ret(ret) if !is_last => {
                    // warning? ret statement should not be in the middle of block
                }
                _ if is_last && !ret_ty.is_void() => {
                    // error if fun_ret_ty is not void
                    return Err(compile_error(pos, TypeCkError::MustBeRetStmt));
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
        let name = hir::Ident::new(fun.name.kind.as_ident().raw.clone(), def.clone().into());
        Ok(hir::Fun::new(
            id,
            name,
            args,
            ret_ty,
            hir::Block::new(fun.block.id, stmts),
            def,
        ))
    }

    fn lower_stmt(
        &self,
        scope: &ScopeTable<ty::Type>,
        id: ast::NodeID,
        stmt: &ast::Stmt,
    ) -> Result<hir::Stmt> {
        let stmt = match stmt {
            ast::Stmt::Expr(expr) => self.lower_expr(scope, id, expr)?.into(),
            ast::Stmt::VarDecl(var_decl) => {
                let expr =
                    self.lower_expr(scope, var_decl.expr.id, &var_decl.expr.kind.as_expr())?;
                let ty = expr.get_type().clone();
                let def = scope
                    .lookup(&var_decl.name.kind.as_ident().raw)
                    .unwrap()
                    .as_var_def();
                hir::VarDecl::new(
                    id,
                    var_decl.name.kind.as_ident().clone(),
                    Box::new(expr),
                    VarDef::new(def.id, ty, def.is_mut),
                )
                .into()
            }
            ast::Stmt::Ret(ret) => {
                let expr = match &ret.expr {
                    Some(expr) => Some(self.lower_expr(scope, expr.id, &expr.kind.as_expr())?),
                    None => None,
                };
                hir::Ret::new(id, expr).into()
            }
        };
        Ok(stmt)
    }

    fn lower_lit(&self, id: ast::NodeID, lit: &ast::Lit) -> Result<hir::Lit> {
        let ty = self.ty_map.get(&id).unwrap();
        let lit_kind = self
            .lower_literal(ty, lit.kind.clone(), &lit.literal)
            .map_err(|err| compile_error(lit.pos, err))?;
        Ok(hir::Lit::new(id, lit_kind, ty.clone()))
    }

    fn lower_literal(
        &self,
        ty: &ty::Type,
        lit_kind: ast::LitKind,
        literal: &str,
    ) -> std::result::Result<hir::LitKind, TypeCkError> {
        let kind = match lit_kind {
            // int literal
            ast::LitKind::Int(base) => match ty {
                // to int
                ty::Type::Int(kind) => match kind {
                    // to i8
                    ty::IntKind::I8 => {
                        let n = parse_int_literal(base, &literal).map_err(|err| {
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
                        let n = parse_int_literal(base, &literal).map_err(|err| {
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
                        let n = parse_int_literal(base, &literal).map_err(|err| {
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
                        let n = parse_int_literal(base, &literal).map_err(|err| {
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

    fn lower_expr(
        &self,
        scope: &ScopeTable<ty::Type>,
        id: ast::NodeID,
        expr: &ast::Expr,
    ) -> Result<hir::Expr> {
        let ty = self.ty_map.get(&id).unwrap().clone();
        match expr {
            ast::Expr::Lit(lit) => self.lower_lit(id, lit).map(|v| v.into()),
            ast::Expr::Ident(ident) => {
                let def = scope.lookup(&ident.raw).unwrap().as_var_def();
                Ok(hir::Ident::new(
                    ident.raw.clone(),
                    VarDef::new(def.id, ty, def.is_mut).into(),
                )
                .into())
            }
            ast::Expr::Binary(binary) => self.lower_binary(scope, id, binary),
            ast::Expr::Unary(unary) => {
                match unary.expr.kind.as_expr() {
                    ast::Expr::Lit(lit) => {
                        match unary.op.kind.as_unary_op().kind {
                            ast::UnaryOpKind::Add => {
                                // + should have number
                                if !ty.is_int() && !ty.is_float() {
                                    return Err(compile_error(
                                        lit.pos,
                                        TypeCkError::InvalidUnaryTypes,
                                    ));
                                }
                                self.lower_lit(unary.expr.id, &lit).map(|v| v.into())
                            }
                            ast::UnaryOpKind::Sub => {
                                // - should have number
                                if !ty.is_int() && !ty.is_float() {
                                    return Err(compile_error(
                                        lit.pos,
                                        TypeCkError::InvalidUnaryTypes,
                                    ));
                                }
                                let mut literal = String::from("-");
                                literal.push_str(&lit.literal);
                                self.lower_literal(&ty, lit.kind.clone(), &literal)
                                    .map(|lit_kind| {
                                        hir::Lit::new(unary.expr.id, lit_kind, ty).into()
                                    })
                                    .map_err(|err| compile_error(lit.pos, err))
                            }
                            ast::UnaryOpKind::Not => {
                                // ! should have bool
                                if !ty.is_bool() {
                                    return Err(compile_error(
                                        lit.pos,
                                        TypeCkError::InvalidUnaryTypes,
                                    ));
                                }
                                self.lower_lit(unary.expr.id, &lit).map(|v| {
                                    // toggle bool value
                                    let mut v = v;
                                    v.kind = hir::LitKind::Bool(!v.kind.as_bool());
                                    v.into()
                                })
                            }
                        }
                    }
                    _ => {
                        let expr =
                            self.lower_expr(scope, unary.expr.id, &unary.expr.kind.as_expr())?;
                        let expr_ty = expr.get_type();
                        match unary.op.kind.as_unary_op().kind {
                            ast::UnaryOpKind::Add | ast::UnaryOpKind::Sub => {
                                // + and - should have number
                                if !expr_ty.is_int() && !expr_ty.is_float() {
                                    return Err(compile_error(
                                        unary.expr.pos(),
                                        TypeCkError::InvalidUnaryTypes,
                                    ));
                                }
                            }
                            ast::UnaryOpKind::Not => {
                                // ! should have bool
                                if !expr_ty.is_bool() {
                                    return Err(compile_error(
                                        unary.expr.pos(),
                                        TypeCkError::InvalidUnaryTypes,
                                    ));
                                }
                            }
                        }
                        Ok(
                            hir::Unary::new(id, unary.op.kind.as_unary_op().kind.clone(), expr)
                                .into(),
                        )
                    }
                }
            }
        }
    }

    fn lower_binary(
        &self,
        scope: &ScopeTable<ty::Type>,
        id: ast::NodeID,
        binary: &ast::Binary,
    ) -> Result<hir::Expr> {
        let lhs = self.lower_expr(scope, binary.lhs.id, &binary.lhs.kind.as_expr())?;
        let rhs = self.lower_expr(scope, binary.rhs.id, &binary.rhs.kind.as_expr())?;
        match binary.op.kind.as_ident().raw.as_str() {
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
        Ok(hir::Binary::new(
            id,
            binary.op.kind.as_ident().raw.clone(),
            lhs,
            rhs,
            self.ty_map.get(&id).unwrap().clone(),
        )
        .into())
    }
}
