use parse::syntax::ast::*;
use std::collections::HashMap;
use thiserror::Error;

#[cfg(test)]
mod tests;

#[derive(Error, Debug)]
pub enum TypeCkError {
    // #[error("unterminated char literal: `{0}`")]
// UnterminatedChar(String),
}

type Result<T> = std::result::Result<T, TypeCkError>;

struct TypeCk {
    ty_map: HashMap<NodeId, Type>,
}

impl TypeCk {
    pub fn new() -> Self {
        TypeCk {
            ty_map: HashMap::new(),
        }
    }

    fn add_type(&mut self, id: NodeId, ty: Type) {
        self.ty_map.insert(id, ty);
    }

    pub fn typecheck(&mut self, expr: &Expr) -> Result<()> {
        let ty = match expr {
            Expr::Lit(lit) => match lit.kind {
                LitKind::Int(_) => Type::Int(IntTy::ISize),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };
        self.add_type(expr.id(), ty);
        Ok(())
    }

    pub fn get_type(&mut self, id: NodeId) -> Option<&Type> {
        self.ty_map.get(&id)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Type {
    Int(IntTy),
    Float(FloatTy),
    Bool,
    String,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum IntTy {
    I8,
    I16,
    I32,
    I64,
    ISize,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum FloatTy {
    F32,
    F64,
}
