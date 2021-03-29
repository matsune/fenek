use crate::scope::Def;
use parse::ast;

pub trait Typed {
    fn get_type(&self) -> Type;
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
    Void,
    Int(IntTy),
    Float(FloatTy),
    Bool,
    String,
}

impl Type {
    pub fn into_int_ty(self) -> IntTy {
        match self {
            Self::Int(v) => v,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    // ISize,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    VarDecl(VarDecl),
}

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Ident(Ident),
    Binary(Binary),
    Unary(Unary),
}

impl Into<Stmt> for Expr {
    fn into(self) -> Stmt {
        Stmt::Expr(self)
    }
}

impl Into<Stmt> for VarDecl {
    fn into(self) -> Stmt {
        Stmt::VarDecl(self)
    }
}

#[derive(Debug)]
pub struct VarDecl {
    pub id: ast::NodeId,
    pub name: ast::Ident,
    pub init: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub id: ast::NodeId,
    pub kind: ast::LitKind,
    pub ty: Type,
}

impl Lit {
    pub fn new(id: ast::NodeId, kind: ast::LitKind, ty: Type) -> Self {
        Self { id, kind, ty }
    }
}

impl Into<Expr> for Lit {
    fn into(self) -> Expr {
        Expr::Lit(self)
    }
}

impl Typed for Lit {
    fn get_type(&self) -> Type {
        self.ty
    }
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub raw: String,
    pub def: Def,
}

impl Ident {
    pub fn new(raw: String, def: Def) -> Self {
        Self { raw, def }
    }
}

impl Typed for Ident {
    fn get_type(&self) -> Type {
        match self.def {
            Def::Var(var_def) => var_def.ty,
        }
    }
}

impl Into<Expr> for Ident {
    fn into(self) -> Expr {
        Expr::Ident(self)
    }
}

impl Typed for Expr {
    fn get_type(&self) -> Type {
        match self {
            Expr::Lit(lit) => lit.get_type(),
            Expr::Ident(ident) => ident.get_type(),
            Expr::Binary(binary) => binary.get_type(),
            Expr::Unary(unary) => unary.get_type(),
        }
    }
}

#[derive(Debug)]
pub struct Binary {
    pub id: ast::NodeId,
    pub op: ast::BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub ty: Type,
}

impl Binary {
    pub fn new(id: ast::NodeId, op: ast::BinOp, lhs: Expr, rhs: Expr, ty: Type) -> Self {
        Binary {
            id,
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            ty,
        }
    }
}

impl Typed for Binary {
    fn get_type(&self) -> Type {
        self.ty
    }
}

impl Into<Expr> for Binary {
    fn into(self) -> Expr {
        Expr::Binary(self)
    }
}

#[derive(Debug)]
pub struct Unary {
    pub id: ast::NodeId,
    pub op: ast::UnaryOp,
    pub expr: Box<Expr>,
    pub ty: Type,
}

impl Unary {
    pub fn new(id: ast::NodeId, op: ast::UnaryOp, expr: Expr, ty: Type) -> Self {
        Unary {
            id,
            op,
            expr: Box::new(expr),
            ty,
        }
    }
}

impl Typed for Unary {
    fn get_type(&self) -> Type {
        self.ty
    }
}

impl Into<Expr> for Unary {
    fn into(self) -> Expr {
        Expr::Unary(self)
    }
}
