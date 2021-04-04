use crate::scope::{Def, FunDef, VarDef};
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
    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }

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
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatTy {
    F32,
    F64,
}

pub struct Fun {
    pub id: ast::NodeId,
    pub name: Ident,
    pub args: FunArgs,
    pub ret_ty: Type,
    pub block: Block,
    pub def: FunDef,
}

impl Fun {
    pub fn new(
        id: ast::NodeId,
        name: Ident,
        args: FunArgs,
        ret_ty: Type,
        block: Block,
        def: FunDef,
    ) -> Self {
        Fun {
            id,
            name,
            args,
            ret_ty,
            block,
            def,
        }
    }
}

pub type FunArgs = Vec<Ident>;

pub struct Block {
    pub id: ast::NodeId,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn new(id: ast::NodeId, stmts: Vec<Stmt>) -> Self {
        Block { id, stmts }
    }
}

macro_rules! Enum {
    ($name:ident [$($Var:ident),*]) => {
        #[derive(Debug)]
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

Enum!(Stmt [VarDecl, Ret, Expr]);

#[derive(Debug)]
pub struct VarDecl {
    pub id: ast::NodeId,
    pub name: ast::Ident,
    pub init: Box<Expr>,
    pub def: VarDef,
}

impl VarDecl {
    pub fn new(id: ast::NodeId, name: ast::Ident, init: Box<Expr>, def: VarDef) -> Self {
        Self {
            id,
            name,
            init,
            def,
        }
    }
}

#[derive(Debug)]
pub struct Ret {
    pub id: ast::NodeId,
    pub expr: Option<Expr>,
}

impl Ret {
    pub fn new(id: ast::NodeId, expr: Option<Expr>) -> Self {
        Self { id, expr }
    }
}

Enum!(Expr [Lit, Ident, Binary, Unary]);

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
        match &self.def {
            Def::Fun(fun_def) => fun_def.ret_ty,
            Def::Var(var_def) => var_def.ty,
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
