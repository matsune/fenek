use super::analyze::{NodeMap, TyAnalyzer};
use ast::Node;
use error::{CompileError, Result, TypeCkError};
use hir::def::*;
use lex::token;
use num_traits::Num;
use pos::Pos;
use std::ops::Deref;
use std::str::FromStr;
use typed_arena::Arena;
use types::infer::InferTyArena;
use types::solver::Solver;
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

pub fn lower(module: ast::Module) -> Result<hir::Module> {
    // lifetime 'lower is this block scope
    let ty_arena = InferTyArena::default();
    let struct_arena = Arena::new();
    let solver = Solver::new(&ty_arena, &struct_arena);
    let def_arena = Arena::new();

    let mut lower = {
        // 1. analyze AST and make hash maps to infer types of each nodes
        let (node_infer_ty_map, node_infer_def_map) =
            TyAnalyzer::new(&def_arena, &solver).analyze_module(&module)?;

        // 2. finalize inferring type into concrete ty::Type
        let (node_ty_map, node_def_map) = {
            let mut node_ty_map = NodeMap::with_capacity(node_infer_ty_map.len());
            for (node_id, infer_ty) in node_infer_ty_map.iter() {
                let final_ty = solver.solve_type(infer_ty).map_err(|err| {
                    let node = ast::visit::find_node(&module, *node_id).unwrap();
                    CompileError::new(node.pos(), err.into())
                })?;
                node_ty_map.insert(*node_id, final_ty);
            }

            let mut node_def_map = NodeMap::with_capacity(node_infer_def_map.len());
            for (node_id, def) in node_infer_def_map.iter() {
                let ty = solver.solve_type(def.ty()).unwrap();
                node_def_map.insert(
                    *node_id,
                    match def {
                        Def::Fun(fun_def) => Def::Fun(DefFun {
                            id: fun_def.id,
                            ty,
                            arg_muts: fun_def.arg_muts.clone(),
                            ret_mut: fun_def.ret_mut,
                        }),
                        Def::Var(var_def) => Def::Var(DefVar {
                            id: var_def.id,
                            ty,
                            is_mut: var_def.is_mut,
                        }),
                    },
                );
            }

            (node_ty_map, node_def_map)
        };

        Lower::new(node_ty_map, node_def_map)
    };

    lower.lower_module(&module)
}

struct Lower {
    ty_map: NodeMap<ty::Type>,
    node_def_map: NodeMap<Def<ty::Type>>,
}

type CurrentRetTy = (bool, ty::Type);

impl Lower {
    fn new(ty_map: NodeMap<ty::Type>, node_def_map: NodeMap<Def<ty::Type>>) -> Self {
        Self {
            ty_map,
            node_def_map,
        }
    }

    fn lower_module(&mut self, module: &ast::Module) -> Result<hir::Module> {
        let mut funs = Vec::new();
        for fun in &module.funs {
            funs.push(self.lower_fun(&fun).map_err(|(id, err)| {
                let pos = ast::visit::find_node(&module, id).unwrap().pos();
                compile_error(pos, err)
            })?);
        }
        Ok(hir::Module { funs })
    }

    fn lower_fun(&mut self, fun: &ast::Fun) -> LowerResult<hir::Fun> {
        let id = fun.id;
        let name = fun.name.raw.clone();
        let def = self.node_def_map.get(&id).unwrap().clone();
        let ret_ty = match &fun.ret_ty {
            Some(ret_ty) => {
                let is_mut = ret_ty.is_mut();
                let ty = self.ty_map.get(&ret_ty.ty.id).unwrap();
                if ty.is_fun() {
                    // returning function type is not supported so far
                    return Err((ret_ty.id, TypeCkError::InvalidReturnType));
                }
                (is_mut, ty.clone())
            }
            None => (false, ty::Type::Void),
        };
        let mut args = Vec::new();
        for arg in &fun.args {
            let raw = arg.name.raw.clone();
            let def = self.node_def_map.get(&arg.id).unwrap().clone();
            args.push(hir::Path { raw, def });
        }
        let block = self.lower_block(&fun.block, &ret_ty)?;
        Ok(hir::Fun {
            id,
            name,
            args,
            block,
            def,
        })
    }

    fn lower_block(
        &self,
        block: &ast::Block,
        current_ret_ty: &CurrentRetTy,
    ) -> LowerResult<hir::Block> {
        let id = block.id;
        let mut stmts = Vec::new();
        let stmts_len = block.stmts.len();
        for (idx, stmt) in block.stmts.iter().enumerate() {
            if matches!(stmt, ast::Stmt::Empty(_)) {
                continue;
            }
            let is_last = idx == stmts_len - 1;
            let stmt = self.lower_stmt(&stmt, is_last, current_ret_ty)?;
            stmts.push(stmt);
        }
        Ok(hir::Block { id, stmts })
    }

    fn is_returning_local_var(&self, expr: &hir::Expr) -> bool {
        match expr {
            // TODO: check scope level if introduced global variable, struct and etc.
            hir::Expr::Path(path) => !path.def.ty().is_ptr(),
            hir::Expr::Call(_) => false,
            hir::Expr::RefExpr(ref_expr) => self.is_returning_local_var(&ref_expr.expr),
            hir::Expr::DerefExpr(deref_expr) => self.is_returning_local_var(&deref_expr.expr),
            _ => unreachable!(),
        }
    }

    fn lower_stmt(
        &self,
        stmt: &ast::Stmt,
        is_last: bool,
        current_ret_ty: &CurrentRetTy,
    ) -> LowerResult<hir::Stmt> {
        let id = stmt.id();
        let stmt: hir::Stmt = match &stmt {
            ast::Stmt::Expr(expr) => self.lower_expr(&expr)?.into(),
            ast::Stmt::VarDecl(var_decl) => {
                let name = var_decl.name.raw.clone();
                let expr = self.lower_expr(&var_decl.init)?;
                let def = self.node_def_map.get(&id).unwrap().clone();
                if matches!(def.ty(), ty::Type::Fun(_) | ty::Type::Void) {
                    return Err((id, TypeCkError::NonBasicVar));
                }
                if var_decl.is_mut() && def.ty().is_ptr() && !expr.is_mutable() {
                    return Err((id, TypeCkError::RequiresMut));
                }
                hir::VarDecl::new(id, name, expr, def).into()
            }
            ast::Stmt::Ret(ret) => {
                let expr = match &ret.expr {
                    Some(expr) => {
                        let expr = self.lower_expr(&expr)?;
                        if current_ret_ty.0 && current_ret_ty.1.is_ptr() {
                            if !expr.is_mutable() {
                                // return type of this function is mutable but expr is not mutable
                                return Err((id, TypeCkError::RequiresMut));
                            } else if self.is_returning_local_var(&expr) {
                                return Err((id, TypeCkError::ReturingLocalVar));
                            }
                        }
                        Some(expr)
                    }
                    None => None,
                };
                hir::Ret { id, expr }.into()
            }
            ast::Stmt::Assign(assign) => {
                let left = self.lower_expr(&assign.left)?;
                if !left.is_lvalue() {
                    return Err((id, TypeCkError::LvalueRequired));
                }
                if !left.is_mutable() {
                    return Err((id, TypeCkError::AssigningReadonly));
                }
                let right = self.lower_expr(&assign.right)?;
                hir::Assign::new(id, left, right).into()
            }
            ast::Stmt::Empty(_) => unreachable!(),
            ast::Stmt::If(if_stmt) => {
                let expr = self.lower_expr(if_stmt.expr.as_ref().unwrap())?;
                let block = self.lower_block(&if_stmt.block, current_ret_ty)?;
                let else_if = match &if_stmt.else_if {
                    Some(e) => Some(Box::new(self.lower_else_if(e, current_ret_ty)?)),
                    None => None,
                };
                hir::IfStmt {
                    id,
                    expr: Some(expr),
                    block,
                    else_if,
                }
                .into()
            }
        };
        match (is_last, stmt.is_terminator()) {
            (true, true) => {}
            (true, false) => {
                // ret_ty should be void
                if !current_ret_ty.1.is_void() {
                    return Err((id, TypeCkError::MustBeRetStmt));
                }
            }
            (false, true) => return Err((id, TypeCkError::RetInMiddle)),
            (false, false) => {}
        }
        Ok(stmt)
    }

    fn lower_else_if(
        &self,
        else_stmt: &ast::IfStmt,
        current_ret_ty: &CurrentRetTy,
    ) -> LowerResult<hir::IfStmt> {
        let id = else_stmt.id;
        let expr = match &else_stmt.expr {
            Some(e) => Some(self.lower_expr(e)?),
            None => None,
        };
        let block = self.lower_block(&else_stmt.block, current_ret_ty)?;
        let else_if = match &else_stmt.else_if {
            Some(e) => Some(Box::new(self.lower_else_if(e, current_ret_ty)?)),
            None => None,
        };
        Ok(hir::IfStmt {
            id,
            expr,
            block,
            else_if,
        })
    }

    fn lower_lit(&self, lit: &ast::Lit) -> LowerResult<hir::Lit> {
        let id = lit.id;
        let ty = self.ty_map.get(&id).unwrap();
        let kind = self
            .lower_literal(ty, &lit.kind, &lit.raw)
            .map_err(|err| (id, err))?;
        Ok(hir::Lit {
            id,
            kind,
            ty: ty.clone(),
        })
    }

    fn lower_literal(
        &self,
        ty: &ty::Type,
        lit_kind: &ast::LitKind,
        literal: &str,
    ) -> Result<hir::LitKind, TypeCkError> {
        let kind = match lit_kind {
            // int literal
            ast::LitKind::Int { base } => match ty {
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

    fn lower_path(&self, path: &ast::Path) -> hir::Path {
        let raw = path.ident.raw.clone();
        let def = self.node_def_map.get(&path.id).unwrap().clone();
        hir::Path { raw, def }
    }

    fn lower_call(&self, call: &ast::Call) -> LowerResult<hir::Call> {
        let path = self.lower_path(&call.path);
        let fn_type = path.def.ty().as_fun();
        let fun_def = path.def.as_fun();
        let mut args = Vec::new();
        for (idx, arg) in call.args.iter().enumerate() {
            let expr = self.lower_expr(&arg)?;
            if fun_def.arg_muts[idx] && fn_type.args[idx].is_ptr() && !expr.is_mutable() {
                return Err((arg.id(), TypeCkError::RequiresMut));
            }
            args.push(expr);
        }
        let is_mut = fun_def.ret_mut;
        Ok(hir::Call { path, args, is_mut })
    }

    fn lower_expr(&self, expr: &ast::Expr) -> LowerResult<hir::Expr> {
        match &expr {
            ast::Expr::Lit(lit) => self.lower_lit(lit).map(|v| v.into()),
            ast::Expr::Path(path) => Ok(self.lower_path(path).into()),
            ast::Expr::Call(call) => self.lower_call(call).map(|v| v.into()),
            ast::Expr::Binary(binary) => self.lower_binary(binary).map(|v| v.into()),
            ast::Expr::Unary(unary) => self.lower_unary(unary),
        }
    }

    fn lower_binary(&self, binary: &ast::Binary) -> LowerResult<hir::Binary> {
        let id = binary.id;
        let lhs = self.lower_expr(&binary.lhs)?;
        let rhs = self.lower_expr(&binary.rhs)?;
        match binary.op.kind {
            ast::BinOpKind::Add
            | ast::BinOpKind::Sub
            | ast::BinOpKind::Mul
            | ast::BinOpKind::Div
            | ast::BinOpKind::Lt
            | ast::BinOpKind::Gt
            | ast::BinOpKind::Le
            | ast::BinOpKind::Ge => match (lhs.get_type(), rhs.get_type()) {
                (ty::Type::Int(lty), ty::Type::Int(rty)) if lty == rty => {}
                (ty::Type::Float(lty), ty::Type::Float(rty)) if lty == rty => {}
                _ => return Err((id, TypeCkError::InvalidBinaryTypes)),
            },
        };
        let ty = self.ty_map.get(&id).unwrap().clone();
        Ok(hir::Binary::new(id, binary.op.kind, lhs, rhs, ty))
    }

    fn lower_unary(&self, unary: &ast::Unary) -> LowerResult<hir::Expr> {
        let id = unary.id;
        let ty = self.ty_map.get(&id).unwrap().clone();
        match unary.expr.deref() {
            ast::Expr::Lit(lit) => {
                match unary.op.kind {
                    ast::UnOpKind::Neg => {
                        // - should have number
                        if !ty.is_int() && !ty.is_float() {
                            return Err((id, TypeCkError::InvalidUnaryTypes));
                        }
                        let mut literal = String::from("-");
                        literal.push_str(&lit.raw);
                        self.lower_literal(&ty, &lit.kind, &literal)
                            .map(|kind| hir::Lit { id, kind, ty }.into())
                            .map_err(|err| (id, err))
                    }
                    ast::UnOpKind::Not => {
                        // ! should have bool
                        if !ty.is_bool() {
                            return Err((id, TypeCkError::InvalidUnaryTypes));
                        }
                        self.lower_lit(&lit).map(|v| {
                            // toggle bool value
                            let mut v = v;
                            v.kind = hir::LitKind::Bool(!v.kind.as_bool());
                            v.into()
                        })
                    }
                    ast::UnOpKind::Ref | ast::UnOpKind::Deref => {
                        Err((id, TypeCkError::LvalueRequired))
                    }
                }
            }
            _ => {
                let expr = self.lower_expr(&unary.expr)?;
                let expr_ty = expr.get_type();
                match unary.op.kind {
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
                        Ok(hir::RefExpr::new(id, expr).into())
                    }
                    ast::UnOpKind::Deref => {
                        if expr_ty.is_void() {
                            return Err((id, TypeCkError::InvalidUnaryTypes));
                        }
                        Ok(hir::DerefExpr::new(id, expr).into())
                    }
                }
            }
        }
    }
}
