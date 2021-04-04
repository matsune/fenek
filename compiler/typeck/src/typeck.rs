use crate::mir;
use crate::mir::Typed;
use crate::scope::*;
use crate::tree::{ArenaTree, TreeNodeIdx};
use error::{CompileError, Pos, TypeCkError};
use parse::ast;
use parse::ast::Node;
use std::collections::HashMap;
#[cfg(test)]
mod tests;

type Result<T> = std::result::Result<T, CompileError>;

pub struct TypeCk {
    node_ty_map: HashMap<ast::NodeId, mir::Type>,
    scope_tree: ArenaTree<ScopeTable>,
    scope_idx: TreeNodeIdx,
    def_id: DefId,
    current_fn_ret_ty: Option<mir::Type>,
}

impl Default for TypeCk {
    fn default() -> Self {
        let mut scope_tree = ArenaTree::default();
        let global_scope = ScopeTable::default();
        let scope_idx = scope_tree.add_node(global_scope, None);
        TypeCk {
            node_ty_map: HashMap::new(),
            scope_tree,
            scope_idx,
            def_id: 0,
            current_fn_ret_ty: None,
        }
    }
}

impl TypeCk {
    fn push_scope(&mut self) {
        let idx = self
            .scope_tree
            .add_node(ScopeTable::default(), Some(self.scope_idx));
        self.scope_idx = idx;
    }

    fn pop_scope(&mut self) {
        let parent_idx = self.scope_tree.get(self.scope_idx).unwrap().parent.unwrap();
        self.scope_idx = parent_idx;
    }

    fn record_type(&mut self, id: ast::NodeId, ty: mir::Type) {
        self.node_ty_map.insert(id, ty);
    }

    pub fn get_type(&mut self, id: ast::NodeId) -> Option<&mir::Type> {
        self.node_ty_map.get(&id)
    }

    fn gen_def_id(&mut self) -> DefId {
        let id = self.def_id;
        self.def_id += 1;
        id
    }

    pub fn typecheck_fun<'a>(&'a mut self, fun: &ast::Fun) -> Result<mir::Fun> {
        let ret_ty = if let Some(ty) = &fun.ret_ty {
            self.get_type_from_name(&ty.raw).ok_or_else(|| {
                self.compile_error(ty.pos, TypeCkError::UndefinedType(ty.raw.clone()))
            })?
        } else {
            mir::Type::Void
        };
        self.current_fn_ret_ty = Some(ret_ty);
        self.push_scope();

        let mut args = Vec::new();
        for arg in &fun.args {
            if self.current_scope().lookup(&arg.name.raw).is_some() {
                return Err(self.compile_error(
                    arg.name.pos,
                    TypeCkError::AlreadyDefinedVariable(arg.name.raw.clone()),
                ));
            }
            let ty = self.get_type_from_name(&arg.ty.raw).ok_or_else(|| {
                self.compile_error(arg.ty.pos, TypeCkError::UndefinedType(arg.ty.raw.clone()))
            })?;
            if ty == mir::Type::Void {
                return Err(self.compile_error(arg.ty.pos, TypeCkError::InvalidType));
            }
            let def = VarDef::new(self.gen_def_id(), ty, false, true);
            self.current_scope_mut()
                .insert_var(arg.name.raw.clone(), def);
            args.push(mir::Ident::new(arg.name.raw.clone(), def.into()));
        }
        let mut stmts = Vec::new();
        for stmt in &fun.block.stmts {
            stmts.push(self.typecheck_stmt(&stmt)?);
        }
        if !ret_ty.is_void() {
            // last statement must be ret
            let last_stmt = stmts
                .last()
                .ok_or_else(|| self.compile_error(fun.pos, TypeCkError::InvalidReturnType))?;
            if !matches!(last_stmt, mir::Stmt::Ret(_)) {
                return Err(self.compile_error(
                    fun.block.stmts.last().unwrap().pos(),
                    TypeCkError::InvalidReturnType,
                ));
            }
        }
        let def = FunDef::new(
            self.gen_def_id(),
            ret_ty,
            args.iter().map(|arg| arg.def.as_var_def().ty).collect(),
        );
        let name = mir::Ident::new(fun.name.raw.clone(), def.clone().into());
        self.pop_scope();
        Ok(mir::Fun::new(
            fun.id,
            name,
            args,
            ret_ty,
            mir::Block::new(fun.block.id, stmts),
            def,
        ))
    }

    fn get_type_from_name(&self, ty: &str) -> Option<mir::Type> {
        let ty = match ty {
            "i8" => mir::Type::Int(mir::IntTy::I8),
            "i16" => mir::Type::Int(mir::IntTy::I16),
            "i32" => mir::Type::Int(mir::IntTy::I32),
            "i64" => mir::Type::Int(mir::IntTy::I64),
            "f32" => mir::Type::Float(mir::FloatTy::F32),
            "f64" => mir::Type::Float(mir::FloatTy::F64),
            "bool" => mir::Type::Bool,
            "string" => mir::Type::String,
            _ => return None,
        };
        Some(ty)
    }

    fn compile_error(&self, pos: Pos, typeck_err: TypeCkError) -> CompileError {
        CompileError::new(pos, Box::new(typeck_err))
    }

    pub fn typecheck_stmt<'a>(&'a mut self, stmt: &ast::Stmt) -> Result<mir::Stmt> {
        match stmt {
            ast::Stmt::Expr(expr) => self.typecheck_expr(expr).map(|expr| expr.into()),
            ast::Stmt::VarDecl(var_decl) => {
                if self.current_scope().lookup(&var_decl.name.raw).is_some() {
                    return Err(self.compile_error(
                        var_decl.name.pos,
                        TypeCkError::AlreadyDefinedVariable(var_decl.name.raw.clone()),
                    ));
                }
                let expr = self.typecheck_expr(&var_decl.init)?;
                let def = VarDef::new(self.gen_def_id(), expr.get_type(), false, false);
                self.current_scope_mut()
                    .insert_var(var_decl.name.raw.clone(), def);
                let var_decl =
                    mir::VarDecl::new(var_decl.id, var_decl.name.clone(), Box::new(expr), def);
                Ok(var_decl.into())
            }
            ast::Stmt::Ret(ret) => {
                let expr = match &ret.expr {
                    Some(expr) => Some(self.typecheck_expr(&expr)?),
                    None => None,
                };
                let ty = if let Some(expr) = &expr {
                    expr.get_type()
                } else {
                    mir::Type::Void
                };
                if self.current_fn_ret_ty.unwrap() != ty {
                    return Err(self.compile_error(ret.pos, TypeCkError::InvalidReturnType));
                }
                Ok(mir::Ret::new(ret.id, expr).into())
            }
        }
    }

    pub fn current_scope(&self) -> &ScopeTable {
        self.scope_tree.get(self.scope_idx).unwrap().get()
    }

    fn current_scope_mut(&mut self) -> &mut ScopeTable {
        self.scope_tree.get_mut(self.scope_idx).unwrap().get_mut()
    }

    pub fn typecheck_expr<'a>(&'a mut self, expr: &ast::Expr) -> Result<mir::Expr> {
        let id = expr.id();
        let e: mir::Expr = match expr {
            ast::Expr::Lit(lit) => {
                let ty = match lit.kind {
                    ast::LitKind::Int(_) => mir::Type::Int(mir::IntTy::I64),
                    ast::LitKind::Float(_) => mir::Type::Float(mir::FloatTy::F64),
                    ast::LitKind::Bool(_) => mir::Type::Bool,
                    ast::LitKind::String(_) => mir::Type::String,
                };
                mir::Lit::new(lit.id, lit.kind.clone(), ty).into()
            }
            ast::Expr::Ident(ident) => match self.current_scope().lookup(&ident.raw) {
                Some(def) => mir::Ident::new(ident.raw.clone(), def.clone()).into(),
                None => {
                    return Err(self.compile_error(
                        ident.pos,
                        TypeCkError::UndefinedVariable(ident.raw.clone()),
                    ))
                }
            },
            ast::Expr::Binary(binary) => self.typecheck_binary(binary)?.into(),
            ast::Expr::Unary(unary) => self.typecheck_unary(unary)?.into(),
        };
        self.record_type(id, e.get_type());
        Ok(e)
    }

    pub fn typecheck_binary(&mut self, binary: &ast::Binary) -> Result<mir::Binary> {
        let lhs = self.typecheck_expr(&binary.lhs)?;
        let rhs = self.typecheck_expr(&binary.rhs)?;
        let ty = match binary.op.symbol.as_str() {
            "+" => self
                .binary_add_type(lhs.get_type(), rhs.get_type())
                .ok_or(TypeCkError::InvalidBinaryTypes),
            "-" => self
                .binary_sub_type(lhs.get_type(), rhs.get_type())
                .ok_or(TypeCkError::InvalidBinaryTypes),
            "*" => self
                .binary_mul_type(lhs.get_type(), rhs.get_type())
                .ok_or(TypeCkError::InvalidBinaryTypes),
            "/" => self
                .binary_div_type(lhs.get_type(), rhs.get_type())
                .ok_or(TypeCkError::InvalidBinaryTypes),
            _ => Err(TypeCkError::InvalidBinaryTypes),
        }
        .map_err(|err| self.compile_error(binary.lhs.pos(), err))?;
        Ok(mir::Binary::new(binary.id, binary.op.clone(), lhs, rhs, ty))
    }

    pub fn typecheck_unary(&mut self, unary: &ast::Unary) -> Result<mir::Unary> {
        let expr = self.typecheck_expr(&unary.expr)?;
        let ty = expr.get_type();
        match unary.op {
            ast::UnaryOp::Add | ast::UnaryOp::Sub => {
                if !ty.is_int() && !ty.is_float() {
                    return Err(
                        self.compile_error(unary.expr.pos(), TypeCkError::InvalidUnaryTypes)
                    );
                }
            }
            ast::UnaryOp::Not => {
                if !ty.is_bool() {
                    return Err(
                        self.compile_error(unary.expr.pos(), TypeCkError::InvalidUnaryTypes)
                    );
                }
            }
        }
        Ok(mir::Unary::new(unary.id, unary.op, expr, ty))
    }

    fn binary_add_type(&self, lhs: mir::Type, rhs: mir::Type) -> Option<mir::Type> {
        match (lhs, rhs) {
            (mir::Type::Int(lty), mir::Type::Int(rty)) if lty == rty => Some(lhs),
            (mir::Type::Float(lty), mir::Type::Float(rty)) if lty == rty => Some(lhs),
            _ => None,
        }
    }

    fn binary_sub_type(&self, lhs: mir::Type, rhs: mir::Type) -> Option<mir::Type> {
        match (lhs, rhs) {
            (mir::Type::Int(lty), mir::Type::Int(rty)) if lty == rty => Some(lhs),
            (mir::Type::Float(lty), mir::Type::Float(rty)) if lty == rty => Some(lhs),
            _ => None,
        }
    }

    fn binary_mul_type(&self, lhs: mir::Type, rhs: mir::Type) -> Option<mir::Type> {
        match (lhs, rhs) {
            (mir::Type::Int(lty), mir::Type::Int(rty)) if lty == rty => Some(lhs),
            (mir::Type::Float(lty), mir::Type::Float(rty)) if lty == rty => Some(lhs),
            _ => None,
        }
    }

    fn binary_div_type(&self, lhs: mir::Type, rhs: mir::Type) -> Option<mir::Type> {
        match (lhs, rhs) {
            (mir::Type::Int(lty), mir::Type::Int(rty)) if lty == rty => Some(lhs),
            (mir::Type::Float(lty), mir::Type::Float(rty)) if lty == rty => Some(lhs),
            _ => None,
        }
    }
}
