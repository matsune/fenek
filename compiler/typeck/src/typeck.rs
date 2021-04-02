use crate::mir;
use crate::mir::Typed;
use crate::scope::*;
use crate::tree::{ArenaTree, TreeNodeIdx};
use parse::ast;
use parse::ast::Node;
use std::collections::HashMap;
use thiserror::Error;
#[cfg(test)]
mod tests;

#[derive(Error, Debug)]
pub enum TypeCkError {
    #[error("already defined variable `{0}`")]
    AlreadyDefinedVariable(String),
    #[error("undefined variable `{0}`")]
    UndefinedVariable(String),
    #[error("invalid binary types")]
    InvalidBinaryTypes,
    #[error("invalid unary types")]
    InvalidUnaryTypes,
}

type Result<T> = std::result::Result<T, TypeCkError>;

pub struct TypeCk {
    pub ty_map: HashMap<ast::NodeId, mir::Type>,
    scope_tree: ArenaTree<ScopeTable>,
    scope_id: TreeNodeIdx,

    def_id: usize,
}

impl TypeCk {
    pub fn new() -> Self {
        let mut scope_tree = ArenaTree::default();
        let scope_id = scope_tree.add_node(ScopeTable::default());
        TypeCk {
            ty_map: HashMap::new(),
            scope_tree,
            scope_id,
            def_id: 0,
        }
    }

    fn add_type(&mut self, id: ast::NodeId, ty: mir::Type) {
        self.ty_map.insert(id, ty);
    }

    fn gen_def_id(&mut self) -> usize {
        let id = self.def_id;
        self.def_id += 1;
        id
    }

    pub fn typecheck_stmt<'a>(&'a mut self, stmt: &ast::Stmt) -> Result<mir::Stmt> {
        match stmt {
            ast::Stmt::Expr(expr) => self.typecheck_expr(expr).map(|expr| expr.into()),
            ast::Stmt::VarDecl(var_decl) => {
                if self.current_scope().lookup(&var_decl.name.raw).is_some() {
                    return Err(TypeCkError::AlreadyDefinedVariable(
                        var_decl.name.raw.clone(),
                    ));
                }
                let expr = self.typecheck_expr(&var_decl.init)?;
                let def = VarDef::new(self.gen_def_id(), expr.get_type(), false);
                self.current_scope_mut()
                    .insert_var(var_decl.name.raw.clone(), def);
                let var_decl = mir::VarDecl::new(
                    var_decl.id,
                    var_decl.name.clone(),
                    Box::new(expr),
                    def.into(),
                );
                Ok(var_decl.into())
            }
        }
    }

    pub fn current_scope(&self) -> &ScopeTable {
        self.scope_tree.get(self.scope_id).unwrap().get()
    }

    fn current_scope_mut(&mut self) -> &mut ScopeTable {
        self.scope_tree.get_mut(self.scope_id).unwrap().get_mut()
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
                Some(def) => mir::Ident::new(ident.raw.clone(), *def).into(),
                None => return Err(TypeCkError::UndefinedVariable(ident.raw.clone())),
            },
            ast::Expr::Binary(binary) => self.typecheck_binary(binary)?.into(),
            ast::Expr::Unary(unary) => self.typecheck_unary(unary)?.into(),
        };
        self.add_type(id, e.get_type());
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
        }?;
        Ok(mir::Binary::new(binary.id, binary.op.clone(), lhs, rhs, ty))
    }

    pub fn typecheck_unary(&mut self, unary: &ast::Unary) -> Result<mir::Unary> {
        let expr = self.typecheck_expr(&unary.expr)?;
        let ty = expr.get_type();
        match unary.op {
            ast::UnaryOp::Add | ast::UnaryOp::Sub => {
                if !ty.is_int() && !ty.is_float() {
                    return Err(TypeCkError::InvalidUnaryTypes);
                }
            }
            ast::UnaryOp::Not => {
                if !ty.is_bool() {
                    return Err(TypeCkError::InvalidUnaryTypes);
                }
            }
        }
        Ok(mir::Unary::new(unary.id, unary.op, expr, ty))
    }

    pub fn get_type(&mut self, id: ast::NodeId) -> Option<&mir::Type> {
        self.ty_map.get(&id)
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
