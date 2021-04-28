use lex::token;
use serde::{Serialize, Serializer};
use serde_json::Result;

#[derive(Serialize)]
pub struct Expr {
    id: ast::NodeId,
    kind: ExprKind,
}

impl From<&ast::Expr> for Expr {
    fn from(k: &ast::Expr) -> Self {
        let kind = ExprKind::from(&k.kind);
        Self { id: k.id, kind }
    }
}

#[derive(Serialize)]
enum ExprKind {
    Var(String),
    Lit(Lit),
    // Binary(BinOp, Box<Expr>, Box<Expr>),
    // Unary(UnaryOp, Box<Expr>),
}

impl From<&ast::ExprKind> for ExprKind {
    fn from(k: &ast::ExprKind) -> Self {
        match k {
            ast::ExprKind::Var(tok) => Self::Var(tok.raw.clone()),
            ast::ExprKind::Lit(lit) => Self::Lit(lit.into()),
            _ => unimplemented!(),
        }
    }
}

#[derive(Serialize)]
struct Lit {
    kind: LitKind,
    token: String,
}

impl From<&ast::Lit> for Lit {
    fn from(lit: &ast::Lit) -> Self {
        let kind = LitKind::from(&lit.kind);
        Lit {
            kind,
            token: lit.token.raw.clone(),
        }
    }
}

#[derive(Serialize)]
enum LitKind {
    Int(IntBase),
    Float,
    Bool,
    String,
}

impl From<&ast::LitKind> for LitKind {
    fn from(kind: &ast::LitKind) -> Self {
        match kind {
            ast::LitKind::Int(base) => Self::Int(base.into()),
            ast::LitKind::Float => Self::Float,
            ast::LitKind::Bool => Self::Bool,
            ast::LitKind::String => Self::String,
        }
    }
}

#[derive(Serialize)]
enum IntBase {
    Binary,
    Octal,
    Decimal,
    Hex,
}

impl From<&token::IntBase> for IntBase {
    fn from(base: &token::IntBase) -> Self {
        match base {
            token::IntBase::Binary => Self::Binary,
            token::IntBase::Octal => Self::Octal,
            token::IntBase::Decimal => Self::Decimal,
            token::IntBase::Hex => Self::Hex,
        }
    }
}
