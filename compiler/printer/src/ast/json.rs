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
    Binary {
        bin_op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        unary_op: UnaryOp,
        expr: Box<Expr>,
    },
}

impl From<&ast::ExprKind> for ExprKind {
    fn from(k: &ast::ExprKind) -> Self {
        match k {
            ast::ExprKind::Var(tok) => Self::Var(tok.raw.clone()),
            ast::ExprKind::Lit(lit) => Self::Lit(lit.into()),
            ast::ExprKind::Binary(op, lhs, rhs) => {
                let lhs: &ast::Expr = &lhs;
                let rhs: &ast::Expr = &rhs;
                Self::Binary {
                    bin_op: op.into(),
                    lhs: Box::new(Expr::from(lhs)),
                    rhs: Box::new(Expr::from(rhs)),
                }
            }
            ast::ExprKind::Unary(op, expr) => {
                let expr: &ast::Expr = &expr;
                Self::Unary {
                    unary_op: op.into(),
                    expr: Box::new(Expr::from(expr)),
                }
            }
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

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl Serialize for BinOp {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(match *self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
        })
    }
}

impl From<&ast::BinOp> for BinOp {
    fn from(op: &ast::BinOp) -> Self {
        match op {
            ast::BinOp::Add => Self::Add,
            ast::BinOp::Sub => Self::Sub,
            ast::BinOp::Mul => Self::Mul,
            ast::BinOp::Div => Self::Div,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Minus,
    Not,
}

impl Serialize for UnaryOp {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(match *self {
            Self::Minus => "-",
            Self::Not => "!",
        })
    }
}

impl From<&ast::UnaryOp> for UnaryOp {
    fn from(op: &ast::UnaryOp) -> Self {
        match op {
            ast::UnaryOp::Minus => Self::Minus,
            ast::UnaryOp::Not => Self::Not,
        }
    }
}
