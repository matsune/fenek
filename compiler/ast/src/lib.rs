use lex::token;
use pos::Offset;
use std::convert::TryFrom;

pub mod visit;

pub type NodeId = usize;

pub struct Module {
    pub funs: Vec<Fun>,
}

impl Module {
    pub fn new(funs: Vec<Fun>) -> Self {
        Self { funs }
    }
}

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

#[derive(Debug)]
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
    pub fn new_basic(id: NodeId, tok: token::Token) -> Self {
        Self {
            id,
            kind: TyKind::Basic(tok),
        }
    }

    pub fn new_ref(id: NodeId, ty: Ty, offset: Offset) -> Self {
        Self {
            id,
            kind: TyKind::Ref(Box::new(ty), offset),
        }
    }

    pub fn offset(&self) -> Offset {
        self.kind.offset()
    }
}

#[derive(Debug)]
pub enum TyKind {
    Basic(token::Token),
    Ref(Box<Ty>, Offset),
    // Lambda, Tuple...
}

impl ToString for TyKind {
    fn to_string(&self) -> String {
        match self {
            Self::Basic(tok) => tok.raw.clone(),
            Self::Ref(ty, _) => format!("&{}", ty.to_string()),
        }
    }
}

impl TyKind {
    pub fn offset(&self) -> Offset {
        match self {
            Self::Basic(tok) => tok.offset,
            Self::Ref(_, offset) => *offset,
        }
    }

    pub fn is_ref(&self) -> bool {
        matches!(self, Self::Ref(_, _))
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

    pub fn new_var_decl(
        id: NodeId,
        keyword: token::Token,
        name: token::Token,
        ty: Option<Ty>,
        init: Expr,
    ) -> Self {
        Self {
            id,
            kind: StmtKind::VarDecl {
                keyword,
                name,
                ty,
                init,
            },
        }
    }

    pub fn new_assign(id: NodeId, left: Expr, right: Expr) -> Self {
        Self {
            id,
            kind: StmtKind::Assign(left, right),
        }
    }

    pub fn new_expr(id: NodeId, expr: Expr) -> Self {
        Self {
            id,
            kind: StmtKind::Expr(expr),
        }
    }

    pub fn new_if(id: NodeId, if_stmt: IfStmt) -> Self {
        Self {
            id,
            kind: StmtKind::If(if_stmt),
        }
    }

    pub fn offset(&self) -> Offset {
        self.kind.offset()
    }
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Expr),
    Ret {
        keyword: token::Token,
        expr: Option<Expr>,
    },
    VarDecl {
        keyword: token::Token,
        name: token::Token,
        ty: Option<Ty>,
        init: Expr,
    },
    Assign(Expr, Expr),
    Empty(Offset),
    If(IfStmt),
}

impl StmtKind {
    pub fn offset(&self) -> Offset {
        use StmtKind::*;
        match self {
            Expr(expr) => expr.offset(),
            Ret { keyword, expr: _ } => keyword.offset,
            VarDecl {
                keyword,
                name: _,
                ty: _,
                init: _,
            } => keyword.offset,
            Assign(l, _) => l.offset(),
            Empty(offset) => *offset,
            If(if_stmt) => if_stmt.offset,
        }
    }
}

#[derive(Debug)]
pub struct IfStmt {
    pub id: NodeId,
    pub offset: Offset,
    pub expr: Option<Expr>,
    pub block: Block,
    pub else_if: Option<Box<IfStmt>>,
}

impl IfStmt {
    pub fn new(
        id: NodeId,
        offset: Offset,
        expr: Option<Expr>,
        block: Block,
        else_if: Option<Box<IfStmt>>,
    ) -> Self {
        Self {
            id,
            offset,
            expr,
            block,
            else_if,
        }
    }
}

#[derive(Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(id: NodeId, kind: ExprKind) -> Self {
        Self { id, kind }
    }

    pub fn new_path(id: NodeId, token: token::Token) -> Self {
        Self {
            id,
            kind: ExprKind::Path(token),
        }
    }

    pub fn new_call(id: NodeId, token: token::Token, args: Vec<Expr>) -> Self {
        Self {
            id,
            kind: ExprKind::Call(token, args),
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
            kind: ExprKind::Unary(UnOp::new(op_raw), expr),
        }
    }

    pub fn offset(&self) -> Offset {
        self.kind.offset()
    }
}

#[derive(Debug)]
pub enum ExprKind {
    // `Path` will have an array of tokens to represent
    // either variable, method or field of struct.
    Path(token::Token),
    Call(token::Token, Vec<Expr>),
    Lit(Lit),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
}

impl ExprKind {
    pub fn offset(&self) -> Offset {
        match self {
            Self::Path(tok) => tok.offset,
            Self::Call(tok, _) => tok.offset,
            Self::Lit(lit) => lit.token.offset,
            Self::Binary(_, lhs, _) => lhs.offset(),
            Self::Unary(op, _) => op.raw.offset,
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Assoc {
    Left,
    Right,
}

impl Assoc {
    pub fn is_left(&self) -> bool {
        matches!(self, Self::Left)
    }
}

#[derive(Debug)]
pub struct UnOp {
    pub raw: token::Token,
}

impl UnOp {
    fn new(raw: token::Token) -> Self {
        Self { raw }
    }

    pub fn op_kind(&self) -> UnOpKind {
        UnOpKind::try_from(self.raw.kind).unwrap()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOpKind {
    // -
    Neg,
    // !
    Not,
    // &
    Ref,
}

impl TryFrom<token::TokenKind> for UnOpKind {
    type Error = &'static str;

    fn try_from(k: token::TokenKind) -> Result<Self, Self::Error> {
        let kind = match k {
            token::TokenKind::Minus => Self::Neg,
            token::TokenKind::Not => Self::Not,
            token::TokenKind::And => Self::Ref,
            _ => return Err("unknown unary op"),
        };
        Ok(kind)
    }
}
