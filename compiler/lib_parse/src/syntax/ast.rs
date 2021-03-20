use crate::lexer;

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Expr { kind }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Binary(BinOpKind, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub struct Ident {
    literal: String,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LitKind {
    Int,
    Float,
    Bool(bool),
    String,
}

impl std::convert::From<lexer::LitKind> for LitKind {
    fn from(kind: lexer::LitKind) -> LitKind {
        match kind {
            lexer::LitKind::Int => LitKind::Int,
            lexer::LitKind::Float => LitKind::Float,
            lexer::LitKind::Bool(b) => LitKind::Bool(b),
            lexer::LitKind::String => LitKind::String,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Lit {
    kind: LitKind,
    literal: String,
}

impl Lit {
    pub fn new(kind: LitKind, literal: String) -> Self {
        Lit { kind, literal }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOpKind {
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
}

impl BinOpKind {
    pub fn precedence(&self) -> u8 {
        use BinOpKind::*;
        match self {
            Add | Sub => 10,
            Mul | Div => 11,
        }
    }
}
