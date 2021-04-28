use lex::token;

pub type NodeId = usize;

// pub type AstArena<'a> = Arena<AstNode<'a>>;

// pub struct AstNode<'a> {
//     pub id: NodeID,
//     pub kind: AstKind<'a>,
// }

// impl<'a> AstNode<'a> {
//     pub fn new(id: NodeID, kind: AstKind<'a>) -> Self {
//         Self { id, kind }
//     }

//     pub fn pos(&self) -> Pos {
//         self.kind.pos()
//     }
// }

// pub enum AstKind<'a> {
//     Fun(Fun<'a>),
//     Block(Block<'a>),
//     Stmt(Stmt<'a>),
//     UnaryOp(UnaryOp),
// }

// impl<'a> AstKind<'a> {
//     pub fn pos(&self) -> Pos {
//         match self {
//             Self::Fun(fun) => fun.name.pos(),
//             Self::Block(block) => block.pos,
//             Self::Stmt(stmt) => stmt.pos(),
//             Self::UnaryOp(unary_op) => unary_op.pos,
//         }
//     }

//     pub fn new_fun(
//         name: &'a AstNode<'a>,
//         args: FunArgs<'a>,
//         ret_ty: Option<&'a AstNode<'a>>,
//         block: &'a AstNode<'a>,
//     ) -> Self {
//         Self::Fun(Fun::new(name, args, ret_ty, block))
//     }

//     pub fn new_ident(raw: String, pos: Pos) -> Self {
//         Self::Stmt(Stmt::Expr(Expr::Ident(Ident::new(raw, pos))))
//     }

//     pub fn new_block(stmts: Vec<&'a AstNode<'a>>, pos: Pos) -> Self {
//         Self::Block(Block::new(stmts, pos))
//     }

//     pub fn new_ret(expr: Option<&'a AstNode<'a>>, pos: Pos) -> Self {
//         Self::Stmt(Stmt::Ret(Ret::new(expr, pos)))
//     }

//     pub fn new_var_decl(name: &'a AstNode<'a>, expr: &'a AstNode<'a>) -> Self {
//         Self::Stmt(Stmt::VarDecl(VarDecl::new(name, expr)))
//     }

//     pub fn new_unary(op: &'a AstNode<'a>, expr: &'a AstNode<'a>) -> Self {
//         Self::Stmt(Stmt::Expr(Expr::Unary(Unary::new(op, expr))))
//     }

//     pub fn new_unary_op(kind: UnaryOpKind, pos: Pos) -> Self {
//         Self::UnaryOp(UnaryOp::new(kind, pos))
//     }

//     pub fn new_lit(kind: LitKind, literal: String, pos: Pos) -> Self {
//         Self::Stmt(Stmt::Expr(Expr::Lit(Lit::new(kind, literal, pos))))
//     }

//     pub fn as_fun(&self) -> &Fun {
//         match self {
//             Self::Fun(fun) => fun,
//             _ => panic!(),
//         }
//     }

//     pub fn as_ident(&self) -> &Ident {
//         match self {
//             Self::Stmt(Stmt::Expr(Expr::Ident(ident))) => ident,
//             _ => panic!(),
//         }
//     }

//     pub fn as_block(&self) -> &Block {
//         match self {
//             Self::Block(block) => block,
//             _ => panic!(),
//         }
//     }

//     pub fn as_stmt(&self) -> &Stmt {
//         match self {
//             Self::Stmt(stmt) => stmt,
//             _ => panic!(),
//         }
//     }

//     pub fn as_expr(&self) -> &Expr {
//         match self {
//             Self::Stmt(Stmt::Expr(expr)) => expr,
//             _ => panic!(),
//         }
//     }

//     pub fn as_unary_op(&self) -> &UnaryOp {
//         match self {
//             Self::UnaryOp(unary_op) => unary_op,
//             _ => panic!(),
//         }
//     }
// }

// pub struct Fun<'a> {
//     // Ident
//     pub name: &'a AstNode<'a>,
//     // Vec FunArg
//     pub args: FunArgs<'a>,
//     // Ident
//     pub ret_ty: Option<&'a AstNode<'a>>,
//     // Block
//     pub block: &'a AstNode<'a>,
// }

// impl<'a> Fun<'a> {
//     pub fn new(
//         name: &'a AstNode<'a>,
//         args: FunArgs<'a>,
//         ret_ty: Option<&'a AstNode<'a>>,
//         block: &'a AstNode<'a>,
//     ) -> Self {
//         Self {
//             name,
//             args,
//             ret_ty,
//             block,
//         }
//     }
// }

// #[derive(Debug, Clone)]
// pub struct Ident {
//     pub raw: String,
//     pub pos: Pos,
// }

// impl Ident {
//     pub fn new(raw: String, pos: Pos) -> Self {
//         Self { raw, pos }
//     }
// }

// pub type FunArgs<'a> = Vec<FunArg<'a>>;

// pub struct FunArg<'a> {
//     // Ident
//     pub name: &'a AstNode<'a>,
//     // Ident
//     pub ty: &'a AstNode<'a>,
// }

// impl<'a> FunArg<'a> {
//     pub fn new(name: &'a AstNode<'a>, ty: &'a AstNode<'a>) -> Self {
//         Self { name, ty }
//     }
// }

// pub struct Block<'a> {
//     // Vec Stmt
//     pub stmts: Vec<&'a AstNode<'a>>,
//     pub pos: Pos,
// }

// impl<'a> Block<'a> {
//     pub fn new(stmts: Vec<&'a AstNode<'a>>, pos: Pos) -> Self {
//         Self { stmts, pos }
//     }
// }

// pub enum Stmt<'a> {
//     Expr(Expr<'a>),
//     Ret(Ret<'a>),
//     VarDecl(VarDecl<'a>),
// }

// impl<'a> Stmt<'a> {
//     pub fn pos(&self) -> Pos {
//         match self {
//             Self::Expr(expr) => expr.pos(),
//             Self::Ret(ret) => ret.pos,
//             Self::VarDecl(var_decl) => var_decl.name.pos(),
//         }
//     }
// }

// pub struct Ret<'a> {
//     // Expr
//     pub expr: Option<&'a AstNode<'a>>,
//     pub pos: Pos,
// }

// impl<'a> Ret<'a> {
//     pub fn new(expr: Option<&'a AstNode<'a>>, pos: Pos) -> Self {
//         Self { expr, pos }
//     }
// }

// pub struct VarDecl<'a> {
//     // Ident
//     pub name: &'a AstNode<'a>,
//     // Expr
//     pub expr: &'a AstNode<'a>,
// }

// impl<'a> VarDecl<'a> {
//     pub fn new(name: &'a AstNode<'a>, expr: &'a AstNode<'a>) -> Self {
//         Self { name, expr }
//     }
// }

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
