use lex::token;
use serde::{Serialize, Serializer};

#[derive(Serialize)]
pub struct Module {
    funs: Vec<Fun>,
}

impl From<&ast::Module> for Module {
    fn from(k: &ast::Module) -> Self {
        Self {
            funs: k.funs.iter().map(|f| f.into()).collect(),
        }
    }
}

#[derive(Serialize)]
pub struct Fun {
    id: ast::NodeId,
    name: String,
    args: FunArgs,
    ret_ty: Option<Ty>,
    block: Block,
}

impl From<&ast::Fun> for Fun {
    fn from(k: &ast::Fun) -> Self {
        Self {
            id: k.id,
            name: k.name.raw.clone(),
            args: k.args.iter().map(|arg| arg.into()).collect(),
            ret_ty: k.ret_ty.as_ref().map(|t| t.into()),
            block: Block::from(&k.block),
        }
    }
}

type FunArgs = Vec<FunArg>;

#[derive(Serialize)]
struct FunArg {
    id: ast::NodeId,
    name: String,
    ty: Ty,
}

impl From<&ast::FunArg> for FunArg {
    fn from(k: &ast::FunArg) -> Self {
        Self {
            id: k.id,
            name: k.name.raw.clone(),
            ty: Ty::from(&k.ty),
        }
    }
}

#[derive(Serialize)]
struct Ty {
    id: ast::NodeId,
    #[serde(rename(serialize = "ty_kind"))]
    kind: TyKind,
}

impl From<&ast::Ty> for Ty {
    fn from(k: &ast::Ty) -> Self {
        Self {
            id: k.id,
            kind: TyKind::from(&k.kind),
        }
    }
}

#[derive(Serialize)]
enum TyKind {
    Basic(String),
    Ptr(Box<TyKind>),
}

impl From<&ast::TyKind> for TyKind {
    fn from(k: &ast::TyKind) -> Self {
        match k {
            ast::TyKind::Basic(tok) => Self::Basic(tok.raw.clone()),
            ast::TyKind::Ptr(ty, _) => Self::Ptr(Box::new((&ty.kind).into())),
        }
    }
}

#[derive(Serialize)]
struct Block {
    id: ast::NodeId,
    stmts: Vec<Stmt>,
}

impl From<&ast::Block> for Block {
    fn from(k: &ast::Block) -> Self {
        Self {
            id: k.id,
            stmts: k.stmts.iter().map(|s| s.into()).collect(),
        }
    }
}

#[derive(Serialize)]
pub struct Stmt {
    id: ast::NodeId,
    #[serde(rename(serialize = "stmt_kind"))]
    kind: StmtKind,
}

impl From<&ast::Stmt> for Stmt {
    fn from(k: &ast::Stmt) -> Self {
        Self {
            id: k.id,
            kind: StmtKind::from(&k.kind),
        }
    }
}

#[derive(Serialize)]
enum StmtKind {
    Expr(Expr),
    Ret(Option<Expr>),
    VarDecl {
        name: String,
        ty: Option<Ty>,
        init: Expr,
    },
    Assign(Expr, Expr),
    Empty,
}

impl From<&ast::StmtKind> for StmtKind {
    fn from(k: &ast::StmtKind) -> Self {
        match k {
            ast::StmtKind::Expr(expr) => Self::Expr(expr.into()),
            ast::StmtKind::Ret { keyword: _, expr } => Self::Ret(expr.as_ref().map(|e| e.into())),
            ast::StmtKind::VarDecl {
                keyword: _,
                name,
                ty,
                init,
            } => Self::VarDecl {
                name: name.raw.clone(),
                ty: ty.as_ref().map(|v| v.into()),
                init: init.into(),
            },
            ast::StmtKind::Assign(left, right) => Self::Assign(left.into(), right.into()),
            ast::StmtKind::Empty(_) => Self::Empty,
        }
    }
}

#[derive(Serialize)]
pub struct Expr {
    id: ast::NodeId,
    #[serde(rename(serialize = "expr_kind"))]
    kind: ExprKind,
}

impl From<&ast::Expr> for Expr {
    fn from(k: &ast::Expr) -> Self {
        Self {
            id: k.id,
            kind: ExprKind::from(&k.kind),
        }
    }
}

#[derive(Serialize)]
enum ExprKind {
    Path {
        raw: String,
    },
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Lit(Lit),
    Binary {
        bin_op: BinOpKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        unary_op: UnOpKind,
        expr: Box<Expr>,
    },
}

impl From<&ast::ExprKind> for ExprKind {
    fn from(k: &ast::ExprKind) -> Self {
        match k {
            ast::ExprKind::Path(tok) => Self::Path {
                raw: tok.raw.clone(),
            },
            ast::ExprKind::Call(tok, args) => Self::Call {
                name: tok.raw.clone(),
                args: args.iter().map(|arg| arg.into()).collect(),
            },
            ast::ExprKind::Lit(lit) => Self::Lit(lit.into()),
            ast::ExprKind::Binary(op, lhs, rhs) => {
                let lhs: &ast::Expr = &lhs;
                let rhs: &ast::Expr = &rhs;
                Self::Binary {
                    bin_op: BinOpKind::from(op.op_kind()),
                    lhs: Box::new(Expr::from(lhs)),
                    rhs: Box::new(Expr::from(rhs)),
                }
            }
            ast::ExprKind::Unary(op, expr) => {
                let expr: &ast::Expr = &expr;
                Self::Unary {
                    unary_op: op.op_kind().into(),
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
        Lit {
            kind: LitKind::from(&lit.kind),
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

pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

impl Serialize for BinOpKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
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

impl From<ast::BinOpKind> for BinOpKind {
    fn from(op: ast::BinOpKind) -> Self {
        match op {
            ast::BinOpKind::Add => Self::Add,
            ast::BinOpKind::Sub => Self::Sub,
            ast::BinOpKind::Mul => Self::Mul,
            ast::BinOpKind::Div => Self::Div,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOpKind {
    Minus,
    Not,
    Ref,
    Deref,
}

impl Serialize for UnOpKind {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(match *self {
            Self::Minus => "-",
            Self::Not => "!",
            Self::Ref => "&",
            Self::Deref => "*",
        })
    }
}

impl From<ast::UnOpKind> for UnOpKind {
    fn from(op: ast::UnOpKind) -> Self {
        use ast::UnOpKind::*;
        match op {
            Minus => Self::Minus,
            Not => Self::Not,
            Ref => Self::Ref,
            Deref => Self::Deref,
        }
    }
}
