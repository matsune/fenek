use super::analyze::TyAnalyzer;
use crate::infer_ty::*;
use crate::scope::*;
use error::{CompileError, Result, TypeCkError};
use hir::def::*;
use hir::ty;
use lex::token;
use num_traits::Num;
use pos::{Pos, SrcFile};
use std::collections::HashMap;
use std::str::FromStr;

fn compile_error(pos: Pos, typeck_err: TypeCkError) -> CompileError {
    CompileError::new(pos, Box::new(typeck_err))
}

// https://github.com/rust-lang/rust/issues/22639
fn is_parse_int_overflow_error(e: std::num::ParseIntError) -> bool {
    let pos_overflow_err = "2147483648".parse::<i32>().err().unwrap();
    if e == pos_overflow_err {
        return true;
    }
    let neg_overflow_err = "-2147483649".parse::<i32>().err().unwrap();
    e == neg_overflow_err
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

fn parse_int_literal<T: Num>(base: token::IntBase, literal: &str) -> Result<T, T::FromStrRadixErr> {
    T::from_str_radix(&strip_base_underscore(base, &literal), base.into())
}

type LowerResult<T> = Result<T, (ast::NodeId, TypeCkError)>;

pub fn lower(src: &SrcFile, module: ast::Module) -> Result<hir::Module> {
    let lower = {
        // lifetime 'infer
        let ty_arena = InferTyArena::default();
        let mut analyzer = TyAnalyzer::new(src, &ty_arena);
        analyzer.analyze_module(&module)?;

        // Finalyze InferTy into ty::Type
        let mut node_ty_map = HashMap::with_capacity(analyzer.node_ty_map.len());
        for (node_id, infer_ty) in analyzer.node_ty_map.iter() {
            let final_ty = analyzer.get_final_type(infer_ty).map_err(|err| {
                let offset =
                    ast::visit::visit_module(&module, *node_id, |node| node.offset()).unwrap();
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
    lower.lower_module(&module)
}

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

    fn lower_module(&self, module: &ast::Module) -> Result<hir::Module> {
        let mut funs = Vec::new();
        for fun in &module.funs {
            funs.push(self.lower_fun(&fun)?);
        }
        Ok(hir::Module::new(funs))
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
                VarDef::new(def.id, def.ty.clone(), def.is_mut).into(),
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
    ) -> Result<hir::LitKind, TypeCkError> {
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
