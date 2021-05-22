use super::analyze::{NodeMap, TyAnalyzer};
use super::arena::*;
use error::{CompileError, Result, TypeCkError};
use hir::def::*;
use lex::token;
use num_traits::Num;
use pos::{Pos, SrcFile};
use std::str::FromStr;
use types::infer::InferTyArena;
use types::solve::Solver;
use types::ty;

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
    // lifetime 'lower is this block scope
    let ty_arena = InferTyArena::default();
    let solver = Solver::new(&ty_arena);
    let def_arena = DefArena::new();

    let mut lower = {
        // 1. analyze AST and make hash maps to infer types of each nodes
        let (node_infer_ty_map, node_infer_def_map) =
            TyAnalyzer::new(src, &solver, &def_arena).analyze_module(&module)?;

        // 2. finalize inferring type into concrete ty::Type
        let (node_ty_map, node_def_map) = {
            let mut node_ty_map = NodeMap::with_capacity(node_infer_ty_map.len());
            for (node_id, infer_ty) in node_infer_ty_map.iter() {
                let final_ty = solver.solve_type(infer_ty).map_err(|err| {
                    let offset = ast::visit::visit_module(&module, *node_id)
                        .unwrap()
                        .offset();
                    CompileError::new(src.pos_from_offset(offset), err.into())
                })?;
                node_ty_map.insert(*node_id, final_ty);
            }

            let mut node_def_map = NodeMap::with_capacity(node_infer_def_map.len());
            for (node_id, def) in node_infer_def_map.iter() {
                let ty = solver.solve_type(def.ty).unwrap();
                node_def_map.insert(*node_id, Def::new(def.id, ty, false));
            }

            (node_ty_map, node_def_map)
        };

        Lower::new(&src, node_ty_map, node_def_map)
    };

    lower.lower_module(&module)
}

struct Lower<'src> {
    src: &'src SrcFile,
    ty_map: NodeMap<ty::Type>,
    node_def_map: NodeMap<Def<ty::Type>>,
    current_fun_ret_ty: Option<ty::Type>,
}

impl<'src> Lower<'src> {
    fn new(
        src: &'src SrcFile,
        ty_map: NodeMap<ty::Type>,
        node_def_map: NodeMap<Def<ty::Type>>,
    ) -> Self {
        Self {
            src,
            ty_map,
            node_def_map,
            current_fun_ret_ty: None,
        }
    }

    fn lower_module(&mut self, module: &ast::Module) -> Result<hir::Module> {
        let mut funs = Vec::new();
        for fun in &module.funs {
            funs.push(self.lower_fun(&fun)?);
        }
        Ok(hir::Module::new(funs))
    }

    fn lower_fun(&mut self, fun: &ast::Fun) -> Result<hir::Fun> {
        let id = fun.id;
        let def = self.node_def_map.get(&id).unwrap();
        let ret_ty = match &fun.ret_ty {
            Some(ty) => self.ty_map.get(&ty.id).unwrap().clone(),
            None => ty::Type::Void,
        };
        self.current_fun_ret_ty = Some(ret_ty);
        let mut args = Vec::new();
        for arg in &fun.args {
            let def = self.node_def_map.get(&arg.id).unwrap();
            args.push(hir::Path::new(arg.name.raw.clone(), def.clone()));
        }
        let block = self.lower_block(&fun.block).map_err(|(id, err)| {
            let offset = ast::visit::visit_fun(&fun, id).unwrap().offset();
            compile_error(self.src.pos_from_offset(offset), err)
        })?;
        Ok(hir::Fun::new(
            id,
            fun.name.raw.clone(),
            args,
            block,
            def.clone(),
        ))
    }

    fn lower_block(&self, block: &ast::Block) -> LowerResult<hir::Block> {
        let mut stmts = Vec::new();
        let stmts_len = block.stmts.len();
        for (idx, stmt) in block.stmts.iter().enumerate() {
            let stmt_offset = stmt.offset();
            if matches!(stmt.kind, ast::StmtKind::Empty(_)) {
                continue;
            }
            let stmt_id = stmt.id;
            let stmt = self.lower_stmt(&stmt)?;
            let is_last = idx == stmts_len - 1;
            match &stmt {
                hir::Stmt::Ret(ret) if is_last => {
                    let mut expr_ty = match &ret.expr {
                        Some(expr) => expr.get_type(),
                        None => ty::Type::Void,
                    };
                    if expr_ty.is_fun() {
                        expr_ty = *expr_ty.into_fun().ret;
                    }
                    if expr_ty != *self.current_fun_ret_ty.as_ref().unwrap() {
                        return Err((ret.id, TypeCkError::InvalidReturnType));
                    }
                }
                hir::Stmt::Ret(_) if !is_last => {
                    // warning? ret statement should not be in the middle of block
                }
                // _ if is_last && !self.current_fun_ret_ty.as_ref().unwrap().is_void() => {
                //     // error if fun_ret_ty is not void
                //     return Err((stmt_id, TypeCkError::MustBeRetStmt));
                // }
                _ => {}
            }
            stmts.push(stmt);
        }
        Ok(hir::Block::new(block.id, stmts))
    }

    fn lower_stmt(&self, stmt: &ast::Stmt) -> LowerResult<hir::Stmt> {
        let id = stmt.id;
        let stmt = match &stmt.kind {
            ast::StmtKind::Expr(expr) => self.lower_expr(&expr)?.into(),
            ast::StmtKind::VarDecl {
                keyword: _,
                name,
                ty: _,
                init,
            } => {
                let expr = self.lower_expr(&init)?;
                let def = self.node_def_map.get(&id).unwrap();
                hir::VarDecl::new(id, name.clone(), expr, def.clone()).into()
            }
            ast::StmtKind::Ret { keyword: _, expr } => {
                let expr = match expr {
                    Some(expr) => Some(self.lower_expr(&expr)?),
                    None => None,
                };
                hir::Ret::new(id, expr).into()
            }
            ast::StmtKind::Assign(left, right) => {
                let l_ty = self.ty_map.get(&left.id).unwrap();
                let r_ty = self.ty_map.get(&right.id).unwrap();
                let is_normal = *r_ty == ty::Type::Ptr(Box::new(l_ty.clone()));
                let left = if is_normal {
                    match &left.kind {
                        ast::ExprKind::Path(tok) => {
                            let def = self.node_def_map.get(&left.id).unwrap();
                            hir::Path::new(tok.raw.clone(), Def::new(def.id, def.ty.clone(), true))
                                .into()
                        }
                        _ => unreachable!(),
                    }
                } else {
                    self.lower_expr(&left)?
                };
                if !left.is_lvalue() {
                    return Err((id, TypeCkError::LvalueRequired));
                }
                let right = self.lower_expr(&right)?;
                hir::Assign::new(id, left, right).into()
            }
            ast::StmtKind::Empty(_) => unreachable!(),
            ast::StmtKind::If(if_stmt) => {
                let expr = self.lower_expr(if_stmt.expr.as_ref().unwrap())?;
                let block = self.lower_block(&if_stmt.block)?;
                let else_if = match &if_stmt.else_if {
                    Some(e) => Some(Box::new(self.lower_else_if(e)?)),
                    None => None,
                };
                hir::IfStmt::new(id, Some(expr), block, else_if).into()
            }
        };
        Ok(stmt)
    }

    fn lower_else_if(&self, else_stmt: &ast::IfStmt) -> LowerResult<hir::IfStmt> {
        let expr = match &else_stmt.expr {
            Some(e) => Some(self.lower_expr(e)?),
            None => None,
        };
        let block = self.lower_block(&else_stmt.block)?;
        let else_if = match &else_stmt.else_if {
            Some(e) => Some(Box::new(self.lower_else_if(e)?)),
            None => None,
        };
        Ok(hir::IfStmt::new(else_stmt.id, expr, block, else_if))
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
                    ty::IntType::I8 => {
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
                    ty::IntType::I16 => {
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
                    ty::IntType::I32 => {
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
                    ty::IntType::I64 => {
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
                    ty::FloatType::F32 => {
                        let v = f32::from_str(&literal)
                            .map_err(|_| TypeCkError::InvalidFloat(literal.to_string()))?;
                        if v == f32::INFINITY {
                            return Err(TypeCkError::InvalidFloat(literal.to_string()));
                        }
                        hir::LitKind::F32(v)
                    }
                    // to f64
                    ty::FloatType::F64 => {
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
                    ty::FloatType::F32 => {
                        let v = f32::from_str(&literal)
                            .map_err(|_| TypeCkError::InvalidFloat(literal.to_string()))?;
                        if v == f32::INFINITY {
                            return Err(TypeCkError::InvalidFloat(literal.to_string()));
                        }
                        hir::LitKind::F32(v)
                    }
                    // to f64
                    ty::FloatType::F64 => {
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

    fn lower_expr(&self, expr: &ast::Expr) -> LowerResult<hir::Expr> {
        let id = expr.id;
        let ty = self.ty_map.get(&id).unwrap().clone();
        match &expr.kind {
            ast::ExprKind::Lit(lit) => self.lower_lit(id, lit).map(|v| v.into()),
            ast::ExprKind::Path(tok) => {
                let def = self.node_def_map.get(&expr.id).unwrap();
                let expr =
                    hir::Path::new(tok.raw.clone(), Def::new(def.id, def.ty.clone(), true)).into();
                let expr = if def.ty.is_ptr() {
                    // implicit deref
                    hir::DerefExpr::new(id, expr).into()
                } else {
                    expr
                };
                Ok(expr)
            }
            ast::ExprKind::Call(path, args) => {
                let path = path.raw.clone();
                let def = self.node_def_map.get(&expr.id).unwrap();
                let mut _args = Vec::new();
                for arg in args {
                    _args.push(self.lower_expr(&arg)?);
                }
                Ok(hir::Call::new(path, _args, def.clone()).into())
            }
            ast::ExprKind::Binary(op, lhs, rhs) => self.lower_binary(id, op, lhs, rhs),
            ast::ExprKind::Unary(op, expr) => {
                match &expr.kind {
                    ast::ExprKind::Lit(lit) => {
                        match op.op_kind() {
                            ast::UnOpKind::Neg => {
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
                            ast::UnOpKind::Not => {
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
                            ast::UnOpKind::Ref => Err((id, TypeCkError::LvalueRequired)),
                        }
                    }
                    _ => {
                        let expr = self.lower_expr(&expr)?;
                        let expr_ty = expr.get_type();
                        match op.op_kind() {
                            ast::UnOpKind::Neg => {
                                // + and - should have number
                                if !expr_ty.is_int() && !expr_ty.is_float() {
                                    return Err((id, TypeCkError::InvalidUnaryTypes));
                                }
                                Ok(hir::NegExpr::new(id, expr).into())
                            }
                            ast::UnOpKind::Not => {
                                // ! should have bool
                                if !expr_ty.is_bool() {
                                    return Err((id, TypeCkError::InvalidUnaryTypes));
                                }
                                Ok(hir::NotExpr::new(id, expr).into())
                            }
                            ast::UnOpKind::Ref => {
                                if expr_ty.is_void() {
                                    return Err((id, TypeCkError::InvalidUnaryTypes));
                                }
                                if !expr.is_lvalue() {
                                    return Err((id, TypeCkError::LvalueRequired));
                                }
                                match expr {
                                    hir::Expr::DerefExpr(deref_expr) => Ok(*deref_expr.expr),
                                    _ => Ok(hir::RefExpr::new(id, expr).into()),
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn lower_binary(
        &self,
        id: ast::NodeId,
        op: &ast::BinOp,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> LowerResult<hir::Expr> {
        let lhs = self.lower_expr(lhs)?;
        let rhs = self.lower_expr(rhs)?;
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
