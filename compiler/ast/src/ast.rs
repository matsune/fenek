use lex::token;
use span::Offset;

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

impl ToString for Ty {
    fn to_string(&self) -> String {
        self.kind.to_string()
    }
}

impl Ty {
    pub fn new_single(id: NodeId, tok: token::Token) -> Self {
        Self {
            id,
            kind: TyKind::Single(tok),
        }
    }

    pub fn offset(&self) -> Offset {
        self.kind.offset()
    }
}

pub enum TyKind {
    Single(token::Token),
    // Lambda, Tuple...
}

impl ToString for TyKind {
    fn to_string(&self) -> String {
        match self {
            Self::Single(tok) => tok.raw.clone(),
        }
    }
}

impl TyKind {
    pub fn offset(&self) -> Offset {
        match self {
            Self::Single(tok) => tok.offset,
        }
    }
}

pub struct Block {
    pub id: NodeId,
    pub lbrace: token::Token,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn new(id: NodeId, lbrace: token::Token, stmts: Vec<Stmt>) -> Self {
        Self { id, lbrace, stmts }
    }

    pub fn offset(&self) -> Offset {
        self.lbrace.offset
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

    pub fn new_empty(id: NodeId, offset: Offset) -> Self {
        Self {
            id,
            kind: StmtKind::Empty(offset),
        }
    }

    pub fn new_ret(id: NodeId, keyword: token::Token, expr: Option<Expr>) -> Self {
        Self {
            id,
            kind: StmtKind::Ret { keyword, expr },
        }
    }

    pub fn new_var_decl(id: NodeId, keyword: token::Token, name: token::Token, init: Expr) -> Self {
        Self {
            id,
            kind: StmtKind::VarDecl {
                keyword,
                name,
                init,
            },
        }
    }

    pub fn offset(&self) -> Offset {
        self.kind.offset()
    }
}

pub enum StmtKind {
    Expr(Expr),
    Ret {
        keyword: token::Token,
        expr: Option<Expr>,
    },
    VarDecl {
        keyword: token::Token,
        name: token::Token,
        init: Expr,
    },
    Empty(Offset),
}

impl StmtKind {
    pub fn offset(&self) -> Offset {
        match self {
            Self::Expr(expr) => expr.offset(),
            Self::Ret { keyword, expr: _ } => keyword.offset,
            Self::VarDecl {
                keyword,
                name: _,
                init: _,
            } => keyword.offset,
            Self::Empty(offset) => *offset,
        }
    }
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

    pub fn new_binary(id: NodeId, op: BinOp, lhs: Box<Expr>, rhs: Box<Expr>) -> Self {
        Self {
            id,
            kind: ExprKind::Binary(op, lhs, rhs),
        }
    }

    pub fn new_unary(id: NodeId, op_raw: token::Token, expr: Box<Expr>) -> Self {
        Self {
            id,
            kind: ExprKind::Unary(UnaryOp::new(op_raw), expr),
        }
    }

    pub fn offset(&self) -> Offset {
        self.kind.offset()
    }
}

pub enum ExprKind {
    Var(token::Token),
    Lit(Lit),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
}

impl ExprKind {
    pub fn offset(&self) -> Offset {
        match self {
            Self::Var(tok) => tok.offset,
            Self::Lit(lit) => lit.token.offset,
            Self::Binary(_, lhs, _) => lhs.offset(),
            Self::Unary(op, _) => op.raw.offset,
        }
    }
}

pub struct Lit {
    pub kind: LitKind,
    pub token: token::Token,
}

impl Lit {
    pub fn new(kind: LitKind, token: token::Token) -> Self {
        Self { kind, token }
    }

    pub fn offset(&self) -> Offset {
        self.token.offset
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

pub struct BinOp {
    pub raw: token::Token,
}

impl BinOp {
    pub fn new(raw: token::Token) -> Self {
        Self { raw }
    }

    pub fn op_kind(&self) -> BinOpKind {
        BinOpKind::from(self.raw.kind)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

impl From<token::TokenKind> for BinOpKind {
    fn from(k: token::TokenKind) -> Self {
        match k {
            token::TokenKind::Plus => BinOpKind::Add,
            token::TokenKind::Minus => BinOpKind::Sub,
            token::TokenKind::Star => BinOpKind::Mul,
            token::TokenKind::Slash => BinOpKind::Div,
            _ => panic!(),
        }
    }
}

impl BinOpKind {
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

pub struct UnaryOp {
    pub raw: token::Token,
}

impl UnaryOp {
    fn new(raw: token::Token) -> Self {
        Self { raw }
    }

    pub fn op_kind(&self) -> UnaryOpKind {
        UnaryOpKind::from(self.raw.kind)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOpKind {
    Minus,
    Not,
}

impl From<token::TokenKind> for UnaryOpKind {
    fn from(k: token::TokenKind) -> Self {
        match k {
            token::TokenKind::Minus => Self::Minus,
            token::TokenKind::Not => Self::Not,
            _ => panic!(),
        }
    }
}
