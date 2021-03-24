use crate::scope::*;
use crate::tree::{ArenaTree, TreeNode, TreeNodeIdx};
use crate::ty::*;
use parse::syntax::ast::*;
use std::collections::HashMap;
use thiserror::Error;
#[cfg(test)]
mod tests;

#[derive(Error, Debug)]
pub enum TypeCkError {
    #[error("already declared variable `{0}`")]
    AlreadyDeclaredVariable(String),
}

type Result<T> = std::result::Result<T, TypeCkError>;

struct TypeCk {
    ty_map: HashMap<NodeId, Type>,
    scope_tree: ArenaTree<Scope>,
    scope_id: TreeNodeIdx,
}

impl TypeCk {
    pub fn new() -> Self {
        let mut scope_tree = ArenaTree::default();
        let scope_id = scope_tree.add_node(Scope::default());
        TypeCk {
            ty_map: HashMap::new(),
            scope_tree,
            scope_id,
        }
    }

    fn add_type(&mut self, id: NodeId, ty: Type) {
        self.ty_map.insert(id, ty);
    }

    pub fn typecheck_stmt(&mut self, stmt: &Stmt) -> Result<Type> {
        match stmt {
            Stmt::Expr(expr) => self.typecheck_expr(expr),
            Stmt::VarDecl(var_decl) => {
                if self.current_scope().lookup(&var_decl.name.raw).is_some() {
                    return Err(TypeCkError::AlreadyDeclaredVariable(
                        var_decl.name.raw.clone(),
                    ));
                }
                let ty = self.typecheck_expr(&var_decl.init)?;
                self.current_scope_mut()
                    .insert_var(var_decl.name.raw.clone(), VarDef::new(ty, false));
                Ok(Type::Void)
            }
        }
    }

    pub fn current_scope(&self) -> &Scope {
        self.scope_tree.get(self.scope_id).unwrap().get()
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scope_tree.get_mut(self.scope_id).unwrap().get_mut()
    }

    pub fn typecheck_expr(&mut self, expr: &Expr) -> Result<Type> {
        let ty = match expr {
            Expr::Lit(lit) => match lit.kind {
                LitKind::Int(_) => Type::Int(IntTy::ISize),
                LitKind::Float(_) => Type::Float(FloatTy::F64),
                LitKind::Bool(_) => Type::Bool,
                LitKind::String(_) => Type::String,
            },
            Expr::Ident(ident) => match self.current_scope().lookup(&ident.raw) {
                Some(Def::Var(var_def)) => var_def.ty,
                None => unimplemented!(),
            },
            _ => unimplemented!(),
        };
        self.add_type(expr.id(), ty);
        Ok(ty)
    }

    pub fn get_type(&mut self, id: NodeId) -> Option<&Type> {
        self.ty_map.get(&id)
    }
}
