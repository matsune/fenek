pub mod def;

use def::*;
use lex::token;
use types::ty;

pub struct Module {
    pub funs: Vec<Fun>,
}

impl Module {
    pub fn new(funs: Vec<Fun>) -> Self {
        Self { funs }
    }
}

pub struct Fun {
    pub id: ast::NodeId,
    pub name: String,
    pub args: FunArgs,
    pub block: Block,
    pub def: Def<ty::Type>,
}

impl Fun {
    pub fn new(
        id: ast::NodeId,
        name: String,
        args: FunArgs,
        block: Block,
        def: Def<ty::Type>,
    ) -> Self {
        Fun {
            id,
            name,
            args,
            block,
            def,
        }
    }
}

pub type FunArgs = Vec<Path>;

pub struct Block {
    pub id: ast::NodeId,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn new(id: ast::NodeId, stmts: Vec<Stmt>) -> Self {
        Block { id, stmts }
    }

    pub fn is_terminated(&self) -> bool {
        self.stmts
            .last()
            .map(|stmt| stmt.is_terminator())
            .unwrap_or(false)
    }
}

macro_rules! Enum {
    ($name:ident [$($Var:ident),*]) => {
        pub enum $name {
            $(
                $Var($Var),
            )*
        }

        $(
            impl Into<$name> for $Var {
                fn into(self) -> $name {
                    $name::$Var(self)
                }
            }
        )*
    }
}

Enum!(Stmt [VarDecl, Ret, Expr, Assign, IfStmt]);

impl Stmt {
    pub fn is_terminator(&self) -> bool {
        match self {
            Self::Ret(_) => true,
            Self::IfStmt(if_stmt) => if_stmt.is_terminator(),
            _ => false,
        }
    }
}

pub struct IfStmt {
    pub id: ast::NodeId,
    pub expr: Option<Expr>,
    pub block: Block,
    pub else_if: Option<Box<IfStmt>>,
}

impl IfStmt {
    pub fn new(
        id: ast::NodeId,
        expr: Option<Expr>,
        block: Block,
        else_if: Option<Box<IfStmt>>,
    ) -> Self {
        Self {
            id,
            expr,
            block,
            else_if,
        }
    }

    pub fn is_terminator(&self) -> bool {
        let is_terminated = self.block.is_terminated();
        match &self.else_if {
            Some(else_if) => is_terminated && else_if.is_terminator(),
            None => is_terminated,
        }
    }
}

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

pub struct VarDecl {
    pub id: ast::NodeId,
    pub name: token::Token,
    pub init: Box<Expr>,
    pub def: Def<ty::Type>,
}

impl VarDecl {
    pub fn new(id: ast::NodeId, name: token::Token, init: Expr, def: Def<ty::Type>) -> Self {
        Self {
            id,
            name,
            init: Box::new(init),
            def,
        }
    }
}

pub struct Ret {
    pub id: ast::NodeId,
    pub expr: Option<Expr>,
}

impl Ret {
    pub fn new(id: ast::NodeId, expr: Option<Expr>) -> Self {
        Self { id, expr }
    }
}

macro_rules! Enum_with_type {
    ($name:ident [$($Var:ident),*]) => {
        #[derive(Debug)]
        pub enum $name {
            $(
                $Var($Var),
            )*
        }

        impl $name {
            pub fn get_type(&self) -> ty::Type {
                match self {
                    $(
                        $name::$Var(v) => v.get_type(),
                    )*
                }
            }
        }

        $(
            impl Into<$name> for $Var {
                fn into(self) -> $name {
                    $name::$Var(self)
                }
            }
        )*
    }
}

Enum_with_type!(Expr [Lit, Path, Call, Binary, RefExpr, DerefExpr, NegExpr, NotExpr]);

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        match self {
            Self::Path(path) => path.def.is_var,
            Self::RefExpr(ref_expr) => ref_expr.expr.is_lvalue(),
            Self::DerefExpr(deref_expr) => deref_expr.expr.is_lvalue(),
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Lit {
    pub id: ast::NodeId,
    pub kind: LitKind,
    pub ty: ty::Type,
}

impl Lit {
    pub fn new(id: ast::NodeId, kind: LitKind, ty: ty::Type) -> Self {
        Self { id, kind, ty }
    }

    pub fn get_type(&self) -> ty::Type {
        self.ty.clone()
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Path {
    pub raw: String,
    pub def: Def<ty::Type>,
}

impl Path {
    pub fn new(raw: String, def: Def<ty::Type>) -> Self {
        Self { raw, def }
    }

    pub fn get_type(&self) -> ty::Type {
        self.def.ty.clone()
    }
}

#[derive(Debug)]
pub struct Call {
    pub path: String,
    pub args: Vec<Expr>,
    pub def: Def<ty::Type>,
}

impl Call {
    pub fn new(path: String, args: Vec<Expr>, def: Def<ty::Type>) -> Self {
        Self { path, args, def }
    }

    pub fn get_type(&self) -> ty::Type {
        self.def.ty.clone()
    }
}

#[derive(Debug)]
pub struct Binary {
    pub id: ast::NodeId,
    pub op: ast::BinOpKind,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub ty: ty::Type,
}

impl Binary {
    pub fn new(id: ast::NodeId, op: ast::BinOpKind, lhs: Expr, rhs: Expr, ty: ty::Type) -> Self {
        Binary {
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

macro_rules! UnaryEnum {
    ($($name:ident),*) => {
        $(
            #[derive(Debug)]
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

UnaryEnum!(RefExpr, DerefExpr, NegExpr, NotExpr);

impl RefExpr {
    pub fn get_type(&self) -> ty::Type {
        ty::Type::Ptr(Box::new(self.expr.get_type()))
    }
}

impl DerefExpr {
    pub fn get_type(&self) -> ty::Type {
        let ty = self.expr.get_type();
        ty.elem_ty().unwrap_or(&ty).clone()
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
