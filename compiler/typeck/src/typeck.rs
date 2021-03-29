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
    #[error("already declared variable `{0}`")]
    AlreadyDeclaredVariable(String),
    #[error("invalid binary types")]
    InvalidBinaryTypes,
}

type Result<T> = std::result::Result<T, TypeCkError>;

pub struct TypeCk {
    pub ty_map: HashMap<ast::NodeId, mir::Type>,
    scope_tree: ArenaTree<ScopeTable>,
    scope_id: TreeNodeIdx,
}

impl TypeCk {
    pub fn new() -> Self {
        let mut scope_tree = ArenaTree::default();
        let scope_id = scope_tree.add_node(ScopeTable::default());
        TypeCk {
            ty_map: HashMap::new(),
            scope_tree,
            scope_id,
        }
    }

    fn add_type(&mut self, id: ast::NodeId, ty: mir::Type) {
        self.ty_map.insert(id, ty);
    }

    pub fn typecheck_stmt<'a>(&'a mut self, stmt: &ast::Stmt) -> Result<mir::Stmt> {
        match stmt {
            ast::Stmt::Expr(expr) => self.typecheck_expr(expr).map(|expr| expr.into()),
            ast::Stmt::VarDecl(var_decl) => {
                if self.current_scope().lookup(&var_decl.name.raw).is_some() {
                    return Err(TypeCkError::AlreadyDeclaredVariable(
                        var_decl.name.raw.clone(),
                    ));
                }
                let expr = self.typecheck_expr(&var_decl.init)?;
                self.current_scope_mut().insert_var(
                    var_decl.name.raw.clone(),
                    VarDef::new(expr.get_type(), false),
                );
                Ok(expr.into())
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
                None => unimplemented!(),
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
            _ => unimplemented!("binary op"),
        }?;
        Ok(mir::Binary::new(binary.id, binary.op.clone(), lhs, rhs, ty))
    }

    pub fn typecheck_unary(&mut self, unary: &ast::Unary) -> Result<mir::Unary> {
        unimplemented!()
        // let ty = match binary.op {
        //     BinOp::Add => {
        //         let lhs_ty = self.typecheck_expr(&binary.lhs)?;
        //         let rhs_ty = self.typecheck_expr(&binary.rhs)?;
        //         if lhs_ty != rhs_ty {
        //             return Err(TypeCkError::InvalidBinaryTypes);
        //         }
        //         lhs_ty
        //     }
        //     _ => unimplemented!("binary op"),
        // };
        // Ok(ty)
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
}
