use serde::Serialize;

pub mod def;

use def::*;
use types::ty;

#[derive(Serialize)]
pub struct Module {
    pub funs: Vec<Fun>,
}

#[derive(Serialize)]
pub struct Fun {
    pub id: ast::NodeId,
    pub name: String,
    pub args: FunArgs,
    pub block: Block,
    pub def: Def<ty::Type>,
}

pub type FunArgs = Vec<Path>;

#[derive(Debug, Serialize)]
pub struct Block {
    pub id: ast::NodeId,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn is_terminated(&self) -> bool {
        self.stmts
            .last()
            .map(|stmt| stmt.is_terminator())
            .unwrap_or(false)
    }
}

macro_rules! stmt_enum {
    ($($Var:ident),*) => {
        #[derive(Debug, Serialize)]
        pub enum Stmt {
            $(
                $Var($Var),
            )*
        }

        $(
            impl Into<Stmt> for $Var {
                fn into(self) -> Stmt {
                    Stmt::$Var(self)
                }
            }
        )*
    }
}

stmt_enum!(VarDecl, Ret, Expr, Assign, IfStmt);

impl Stmt {
    pub fn is_terminator(&self) -> bool {
        match self {
            Self::Ret(_) => true,
            Self::IfStmt(if_stmt) => if_stmt.is_terminator(),
            _ => false,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct IfStmt {
    pub id: ast::NodeId,
    pub expr: Option<Expr>,
    pub block: Block,
    pub else_if: Option<Box<IfStmt>>,
}

impl IfStmt {
    pub fn is_terminator(&self) -> bool {
        let is_terminated = self.block.is_terminated();
        match &self.else_if {
            Some(else_if) => is_terminated && else_if.is_terminator(),
            None => is_terminated,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct Assign {
    pub id: ast::NodeId,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl Assign {
    pub fn new(id: ast::NodeId, left: Expr, right: Expr) -> Self {
        Self {
            id,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct VarDecl {
    pub id: ast::NodeId,
    pub name: String,
    pub init: Box<Expr>,
    pub def: Def<ty::Type>,
}

impl VarDecl {
    pub fn new(id: ast::NodeId, name: String, init: Expr, def: Def<ty::Type>) -> Self {
        Self {
            id,
            name,
            init: Box::new(init),
            def,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct Ret {
    pub id: ast::NodeId,
    pub expr: Option<Expr>,
}

macro_rules! expr_enum {
    ($($Var:ident),*) => {
        #[derive(Debug, Serialize)]
        pub enum Expr {
            $(
                $Var($Var),
            )*
        }

        impl Expr {
            pub fn get_type(&self) -> ty::Type {
                match self {
                    $(
                        Self::$Var(v) => v.get_type(),
                    )*
                }
            }
        }

        $(
            impl Into<Expr> for $Var {
                fn into(self) -> Expr {
                    Expr::$Var(self)
                }
            }
        )*
    }
}

expr_enum!(Lit, Path, Call, Binary, RefExpr, DerefExpr, NegExpr, NotExpr);

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        match self {
            Self::Path(path) => path.def.is_var(),
            Self::DerefExpr(_) => true,
            _ => false,
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            Self::Call(call) => call.is_mut,
            Self::Path(path) => path.def.is_mutable(),
            Self::DerefExpr(deref_expr) => deref_expr.expr.is_mutable(),
            Self::RefExpr(ref_expr) => ref_expr.expr.is_mutable(),
            _ => false,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct Lit {
    pub id: ast::NodeId,
    pub kind: LitKind,
    pub ty: ty::Type,
}

impl Lit {
    pub fn get_type(&self) -> ty::Type {
        self.ty.clone()
    }
}

#[derive(Debug, Serialize)]
pub enum LitKind {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
}

impl LitKind {
    pub fn as_bool(&self) -> bool {
        if let Self::Bool(b) = self {
            return *b;
        }
        panic!()
    }
}

#[derive(Debug, Serialize)]
pub struct Path {
    pub raw: String,
    pub def: Def<ty::Type>,
}

impl Path {
    pub fn get_type(&self) -> ty::Type {
        self.def.ty.clone()
    }
}

#[derive(Debug, Serialize)]
pub struct Call {
    pub path: Path,
    pub args: Vec<Expr>,
    pub is_mut: bool,
}

impl Call {
    pub fn get_type(&self) -> ty::Type {
        *self.path.get_type().as_fun().ret.clone()
    }
}

#[derive(Debug, Serialize)]
pub struct Binary {
    pub id: ast::NodeId,
    pub op: ast::BinOpKind,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub ty: ty::Type,
}

impl Binary {
    pub fn new(id: ast::NodeId, op: ast::BinOpKind, lhs: Expr, rhs: Expr, ty: ty::Type) -> Self {
        Self {
            id,
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            ty,
        }
    }

    pub fn get_type(&self) -> ty::Type {
        self.ty.clone()
    }
}

macro_rules! unary_expr {
    ($($name:ident),*) => {
        $(
            #[derive(Debug, Serialize)]
            pub struct $name {
                pub id: ast::NodeId,
                pub expr: Box<Expr>,
            }

            impl $name {
                pub fn new(id: ast::NodeId, expr: Expr) -> Self {
                    Self {
                        id,
                        expr: Box::new(expr),
                    }
                }
            }
        )*
    }
}

unary_expr!(RefExpr, DerefExpr, NegExpr, NotExpr);

impl RefExpr {
    pub fn get_type(&self) -> ty::Type {
        ty::Type::Ptr(Box::new(self.expr.get_type()))
    }
}

impl DerefExpr {
    pub fn get_type(&self) -> ty::Type {
        let ty = self.expr.get_type();
        match ty {
            ty::Type::Ptr(ty) => *ty,
            _ => panic!("non-pointer type"),
        }
    }
}

impl NegExpr {
    pub fn get_type(&self) -> ty::Type {
        self.expr.get_type()
    }
}

impl NotExpr {
    pub fn get_type(&self) -> ty::Type {
        self.expr.get_type()
    }
}
