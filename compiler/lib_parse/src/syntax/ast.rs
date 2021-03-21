use crate::lexer;

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Expr { kind }
    }

    pub fn new_lit(kind: LitKind, raw: String) -> Self {
        Expr {
            kind: ExprKind::Lit(Lit::new(kind, raw)),
        }
    }

    pub fn new_ident(raw: String) -> Self {
        Expr {
            kind: ExprKind::Ident(Ident::new(raw)),
        }
    }

    pub fn new_binary(op: BinOpKind, lhs: Expr, rhs: Expr) -> Self {
        Expr {
            kind: ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
        }
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
    raw: String,
}

impl Ident {
    fn new(raw: String) -> Self {
        Ident { raw }
    }
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
    pub kind: LitKind,
    pub raw: String,
}

impl Lit {
    pub fn new(kind: LitKind, raw: String) -> Self {
        Lit { kind, raw }
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
            Mul | Div => 20,
        }
    }
}
