use lex::token;
use pos::Pos;
use std::convert::TryFrom;

pub mod visit;

pub type NodeId = usize;

#[derive(Debug)]
pub struct Module {
    pub funs: Vec<Fun>,
}

#[derive(Debug)]
pub struct Ident {
    pub id: NodeId,
    pub raw: String,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct KwIdent {
    pub id: NodeId,
    pub kind: token::Keyword,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct Fun {
    pub id: NodeId,
    pub keyword: KwIdent,
    pub name: Ident,
    pub args: FunArgs,
    pub ret_ty: Option<RetTy>,
    pub block: Block,
}

impl Fun {
    pub fn pos(&self) -> Pos {
        self.keyword.pos
    }
}

pub type FunArgs = Vec<FunArg>;

#[derive(Debug)]
pub struct FunArg {
    pub id: NodeId,
    pub keyword: Option<KwIdent>,
    pub name: Ident,
    pub ty: Ty,
}

impl FunArg {
    pub fn pos(&self) -> Pos {
        self.keyword
            .as_ref()
            .map(|k| k.pos)
            .unwrap_or(self.name.pos)
    }
}

#[derive(Debug)]
pub struct RetTy {
    pub id: NodeId,
    pub keyword: Option<KwIdent>,
    pub ty: Ty,
}

impl RetTy {
    pub fn pos(&self) -> Pos {
        self.keyword
            .as_ref()
            .map(|k| k.pos)
            .unwrap_or_else(|| self.ty.pos())
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
    pub fn new_basic(id: NodeId, ident: Ident) -> Self {
        Self {
            id,
            kind: TyKind::Basic(ident),
        }
    }

    pub fn new_ptr(id: NodeId, ty: Ty) -> Self {
        Self {
            id,
            kind: TyKind::Ptr(Box::new(ty)),
        }
    }

    pub fn pos(&self) -> Pos {
        match &self.kind {
            TyKind::Basic(ident) => ident.pos,
            TyKind::Ptr(ty) => ty.pos(),
        }
    }
}

#[derive(Debug)]
pub enum TyKind {
    Basic(Ident),
    Ptr(Box<Ty>),
}

impl ToString for TyKind {
    fn to_string(&self) -> String {
        match self {
            Self::Basic(tok) => tok.raw.clone(),
            Self::Ptr(ty) => format!("{}*", ty.to_string()),
        }
    }
}

impl TyKind {
    pub fn is_ptr(&self) -> bool {
        matches!(self, Self::Ptr(_))
    }
}

#[derive(Debug)]
pub struct Block {
    pub id: NodeId,
    pub stmts: Vec<Stmt>,
    pub pos: Pos,
}

impl Block {
    pub fn pos(&self) -> Pos {
        self.pos
    }
}

#[derive(Debug)]
pub enum Stmt {
    Empty(EmptyStmt),
    Expr(Expr),
    Ret(RetStmt),
    VarDecl(VarDecl),
    Assign(Assign),
    If(IfStmt),
}

#[derive(Debug)]
pub struct EmptyStmt {
    pub id: NodeId,
    pub pos: Pos,
}

impl Stmt {
    pub fn id(&self) -> NodeId {
        match self {
            Self::Empty(inner) => inner.id,
            Self::Expr(inner) => inner.id(),
            Self::Ret(inner) => inner.id,
            Self::VarDecl(inner) => inner.id,
            Self::Assign(inner) => inner.id,
            Self::If(inner) => inner.id,
        }
    }

    pub fn pos(&self) -> Pos {
        match self {
            Self::Empty(inner) => inner.pos,
            Self::Expr(inner) => inner.pos(),
            Self::Ret(inner) => inner.pos(),
            Self::VarDecl(inner) => inner.pos(),
            Self::Assign(inner) => inner.pos(),
            Self::If(inner) => inner.pos(),
        }
    }
}

#[derive(Debug)]
pub struct RetStmt {
    pub id: NodeId,
    pub keyword: KwIdent,
    pub expr: Option<Expr>,
}

impl RetStmt {
    pub fn pos(&self) -> Pos {
        self.keyword.pos
    }
}

#[derive(Debug)]
pub struct VarDecl {
    pub id: NodeId,
    pub keyword: KwIdent,
    pub name: Ident,
    pub ty: Option<Ty>,
    pub init: Expr,
}

impl VarDecl {
    pub fn pos(&self) -> Pos {
        self.keyword.pos
    }
}

#[derive(Debug)]
pub struct Assign {
    pub id: NodeId,
    pub left: Expr,
    pub right: Expr,
}

impl Assign {
    pub fn pos(&self) -> Pos {
        self.left.pos()
    }
}

#[derive(Debug)]
pub struct IfStmt {
    pub id: NodeId,
    pub keyword: KwIdent,
    pub expr: Option<Expr>,
    pub block: Block,
    pub else_if: Option<Box<IfStmt>>,
}

impl IfStmt {
    pub fn pos(&self) -> Pos {
        self.keyword.pos
    }
}

#[derive(Debug)]
pub enum Expr {
    Path(Path),
    Call(Call),
    Lit(Lit),
    Binary(Binary),
    Unary(Unary),
}

impl Expr {
    pub fn id(&self) -> NodeId {
        match self {
            Self::Path(inner) => inner.id,
            Self::Call(inner) => inner.id,
            Self::Lit(inner) => inner.id,
            Self::Binary(inner) => inner.id,
            Self::Unary(inner) => inner.id,
        }
    }

    pub fn pos(&self) -> Pos {
        match self {
            Self::Path(inner) => inner.pos(),
            Self::Call(inner) => inner.pos(),
            Self::Lit(inner) => inner.pos,
            Self::Binary(inner) => inner.pos(),
            Self::Unary(inner) => inner.pos(),
        }
    }
}

#[derive(Debug)]
pub struct Path {
    pub id: NodeId,
    // TODO: should be Vec<Ident> to represent member access
    // like `obj.field1.field2`
    pub ident: Ident,
}

impl Path {
    pub fn pos(&self) -> Pos {
        self.ident.pos
    }
}

#[derive(Debug)]
pub struct Call {
    pub id: NodeId,
    pub path: Path,
    pub args: Vec<Expr>,
}

impl Call {
    pub fn pos(&self) -> Pos {
        self.path.pos()
    }
}

#[derive(Debug)]
pub struct Lit {
    pub id: NodeId,
    pub kind: token::LitKind,
    pub raw: String,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct Binary {
    pub id: NodeId,
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl Binary {
    pub fn pos(&self) -> Pos {
        self.lhs.pos()
    }
}

#[derive(Debug)]
pub struct BinOp {
    pub id: NodeId,
    pub kind: BinOpKind,
    pub pos: Pos,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Le,
    Ge,
}

impl TryFrom<token::TokenKind> for BinOpKind {
    type Error = &'static str;

    fn try_from(k: token::TokenKind) -> Result<Self, Self::Error> {
        let kind = match k {
            token::TokenKind::Plus => BinOpKind::Add,
            token::TokenKind::Minus => BinOpKind::Sub,
            token::TokenKind::Star => BinOpKind::Mul,
            token::TokenKind::Slash => BinOpKind::Div,
            token::TokenKind::Lt => BinOpKind::Lt,
            token::TokenKind::Gt => BinOpKind::Gt,
            token::TokenKind::Le => BinOpKind::Le,
            token::TokenKind::Ge => BinOpKind::Ge,
            _ => return Err("not binary op token"),
        };
        Ok(kind)
    }
}

impl BinOpKind {
    pub fn precedence(&self) -> u8 {
        match self {
            Self::Add | Self::Sub => 20,
            Self::Mul | Self::Div => 30,
            Self::Lt | Self::Gt | Self::Le | Self::Ge => 10,
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
pub struct Unary {
    pub id: NodeId,
    pub op: UnOp,
    pub expr: Box<Expr>,
}

impl Unary {
    pub fn pos(&self) -> Pos {
        self.op.pos
    }
}

#[derive(Debug)]
pub struct UnOp {
    pub id: NodeId,
    pub kind: UnOpKind,
    pub pos: Pos,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOpKind {
    // -
    Neg,
    // !
    Not,
    // &
    Ref,
    // *
    Deref,
}

impl TryFrom<token::TokenKind> for UnOpKind {
    type Error = &'static str;

    fn try_from(k: token::TokenKind) -> Result<Self, Self::Error> {
        let kind = match k {
            token::TokenKind::Minus => Self::Neg,
            token::TokenKind::Not => Self::Not,
            token::TokenKind::And => Self::Ref,
            token::TokenKind::Star => Self::Deref,
            _ => return Err("unknown unary op"),
        };
        Ok(kind)
    }
}
