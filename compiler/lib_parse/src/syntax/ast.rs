use crate::lexer;

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Expr),
    VarDecl(VarDecl),
}

impl Stmt {
    pub fn new(kind: StmtKind) -> Self {
        Stmt { kind }
    }
}

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

    pub fn new_binary(op: BinOp, lhs: Expr, rhs: Expr) -> Self {
        Expr {
            kind: ExprKind::Binary(Binary::new(op, lhs, rhs)),
        }
    }

    pub fn new_unary(op: UnaryOp, expr: Expr) -> Self {
        Expr {
            kind: ExprKind::Unary(Unary::new(op, expr)),
        }
    }

    pub fn as_lit(self) -> Lit {
        match self.kind {
            ExprKind::Lit(lit) => lit,
            _ => panic!("failed to unwrap as lit"),
        }
    }

    pub fn as_ident(self) -> Ident {
        match self.kind {
            ExprKind::Ident(ident) => ident,
            _ => panic!("failed to unwrap as ident"),
        }
    }

    pub fn as_binary(self) -> Binary {
        match self.kind {
            ExprKind::Binary(binary) => binary,
            _ => panic!("failed to unwrap as binary"),
        }
    }

    pub fn as_unary(self) -> Unary {
        match self.kind {
            ExprKind::Unary(unary) => unary,
            _ => panic!("failed to unwrap as unary"),
        }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Binary(Binary),
    Unary(Unary),
}

#[derive(Debug, PartialEq)]
pub struct Ident {
    pub raw: String,
}

impl Ident {
    pub fn new(raw: String) -> Self {
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
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Binary {
    pub fn new(op: BinOp, lhs: Expr, rhs: Expr) -> Self {
        Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    /// +
    Add,
    /// -
    Sub,
    /// *
    Mul,
    /// /
    Div,
}

impl BinOp {
    pub fn precedence(&self) -> u8 {
        use BinOp::*;
        match self {
            Add | Sub => 10,
            Mul | Div => 20,
        }
    }
}

#[derive(Debug)]
pub struct Unary {
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

impl Unary {
    pub fn new(op: UnaryOp, expr: Expr) -> Self {
        Unary {
            op,
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    /// +
    Add,
    /// -
    Sub,
    /// !
    Not,
}

/// let <name> = <expr>;
#[derive(Debug)]
pub struct VarDecl {
    pub name: Ident,
    pub init: Box<Expr>,
}

impl VarDecl {
    pub fn new(name: Ident, init: Expr) -> Self {
        VarDecl {
            name,
            init: Box::new(init),
        }
    }
}
