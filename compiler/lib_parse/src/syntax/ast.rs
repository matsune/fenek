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
            kind: ExprKind::Binary(Binary::new(op, lhs, rhs)),
        }
    }

    pub fn unwrap_lit(self) -> Lit {
        match self.kind {
            ExprKind::Lit(lit) => lit,
            _ => panic!("failed to unwrap as lit"),
        }
    }

    pub fn unwrap_ident(self) -> Ident {
        match self.kind {
            ExprKind::Ident(ident) => ident,
            _ => panic!("failed to unwrap as ident"),
        }
    }

    pub fn unwrap_binary(self) -> Binary {
        match self.kind {
            ExprKind::Binary(binary) => binary,
            _ => panic!("failed to unwrap as binary"),
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Binary(Binary),
}

#[derive(Debug, PartialEq)]
pub struct Ident {
    pub raw: String,
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

#[derive(Debug)]
pub struct Binary {
    pub op: BinOpKind,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Binary {
    pub fn new(op: BinOpKind, lhs: Expr, rhs: Expr) -> Self {
        Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
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
