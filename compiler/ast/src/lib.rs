use lex::token;

pub type NodeId = usize;

pub struct Fun {
    pub id: NodeId,
    pub name: token::Token,
    pub args: FunArgs,
    pub ret_ty: Option<Ty>,
    pub block: Block,
}

impl Fun {
    pub fn new(
        id: NodeId,
        name: token::Token,
        args: FunArgs,
        ret_ty: Option<Ty>,
        block: Block,
    ) -> Self {
        Self {
            id,
            name,
            args,
            ret_ty,
            block,
        }
    }
}

pub type FunArgs = Vec<FunArg>;

pub struct FunArg {
    pub id: NodeId,
    pub name: token::Token,
    pub ty: Ty,
}

impl FunArg {
    pub fn new(id: NodeId, name: token::Token, ty: Ty) -> Self {
        Self { id, name, ty }
    }
}

pub struct Ty {
    pub id: NodeId,
    pub kind: TyKind,
}

impl Ty {
    pub fn new_single(id: NodeId, tok: token::Token) -> Self {
        Self {
            id,
            kind: TyKind::Single(tok),
        }
    }
}

pub enum TyKind {
    Single(token::Token),
    // Lambda, Tuple...
}

pub struct Block {
    pub id: NodeId,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn new(id: NodeId, stmts: Vec<Stmt>) -> Self {
        Self { id, stmts }
    }
}

pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(id: NodeId, kind: StmtKind) -> Self {
        Self { id, kind }
    }

    pub fn new_empty(id: NodeId) -> Self {
        Self {
            id,
            kind: StmtKind::Empty,
        }
    }
}

pub enum StmtKind {
    Expr(Expr),
    Ret(Option<Expr>),
    VarDecl(token::Token, Expr),
    Empty,
}

pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(id: NodeId, kind: ExprKind) -> Self {
        Self { id, kind }
    }

    pub fn new_var(id: NodeId, token: token::Token) -> Self {
        Self {
            id,
            kind: ExprKind::Var(token),
        }
    }

    pub fn new_lit(id: NodeId, lit_kind: LitKind, token: token::Token) -> Self {
        Self {
            id,
            kind: ExprKind::Lit(Lit::new(lit_kind, token)),
        }
    }

    pub fn new_binary(id: NodeId, bin_op: BinOp, lhs: Box<Expr>, rhs: Box<Expr>) -> Self {
        Self {
            id,
            kind: ExprKind::Binary(bin_op, lhs, rhs),
        }
    }

    pub fn new_unary(id: NodeId, unary_op: UnaryOp, expr: Box<Expr>) -> Self {
        Self {
            id,
            kind: ExprKind::Unary(unary_op, expr),
        }
    }
}

pub enum ExprKind {
    Var(token::Token),
    Lit(Lit),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
}

pub struct Lit {
    pub kind: LitKind,
    pub token: token::Token,
}

impl Lit {
    pub fn new(kind: LitKind, token: token::Token) -> Self {
        Self { kind, token }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LitKind {
    Int(token::IntBase),
    Float,
    Bool,
    String,
}

impl From<token::LitKind> for LitKind {
    fn from(kind: token::LitKind) -> Self {
        match kind {
            token::LitKind::Int { base } => Self::Int(base),
            token::LitKind::Float => Self::Float,
            token::LitKind::Bool => Self::Bool,
            token::LitKind::String => Self::String,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl From<token::TokenKind> for BinOp {
    fn from(k: token::TokenKind) -> Self {
        match k {
            token::TokenKind::Plus => BinOp::Add,
            token::TokenKind::Minus => BinOp::Sub,
            token::TokenKind::Star => BinOp::Mul,
            token::TokenKind::Slash => BinOp::Div,
            _ => panic!(),
        }
    }
}

impl BinOp {
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Add => 10,
            Self::Sub => 10,
            Self::Mul => 20,
            Self::Div => 20,
        }
    }

    pub fn assoc(&self) -> Assoc {
        Assoc::Left
    }
}

pub enum Assoc {
    Left,
    Right,
}

impl Assoc {
    pub fn is_left(&self) -> bool {
        matches!(self, Self::Left)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Minus,
    Not,
}

impl From<token::TokenKind> for UnaryOp {
    fn from(k: token::TokenKind) -> Self {
        match k {
            token::TokenKind::Minus => Self::Minus,
            token::TokenKind::Not => Self::Not,
            _ => panic!(),
        }
    }
}
