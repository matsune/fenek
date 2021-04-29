pub mod def;
pub mod ty;

use def::*;
use lex::token;

pub trait Typed {
    fn get_type(&self) -> &ty::Type;
}

#[derive(Debug)]
pub struct Fun {
    pub id: ast::NodeId,
    pub name: Ident,
    pub args: FunArgs,
    pub ret_ty: ty::Type,
    pub block: Block,
    pub def: FunDef<ty::Type>,
}

impl Fun {
    pub fn new(
        id: ast::NodeId,
        name: Ident,
        args: FunArgs,
        ret_ty: ty::Type,
        block: Block,
        def: FunDef<ty::Type>,
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

#[derive(Debug)]
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
    pub name: token::Token,
    pub init: Box<Expr>,
    pub def: VarDef<ty::Type>,
}

impl VarDecl {
    pub fn new(
        id: ast::NodeId,
        name: token::Token,
        init: Box<Expr>,
        def: VarDef<ty::Type>,
    ) -> Self {
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
    fn get_type(&self) -> &ty::Type {
        match self {
            Expr::Lit(lit) => lit.get_type(),
            Expr::Ident(ident) => ident.get_type(),
            Expr::Binary(binary) => binary.get_type(),
            Expr::Unary(unary) => unary.get_type(),
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
}

impl Typed for Lit {
    fn get_type(&self) -> &ty::Type {
        &self.ty
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
pub struct Ident {
    pub raw: String,
    pub def: Def<ty::Type>,
}

impl Ident {
    pub fn new(raw: String, def: Def<ty::Type>) -> Self {
        Self { raw, def }
    }
}

impl Typed for Ident {
    fn get_type(&self) -> &ty::Type {
        match &self.def {
            Def::Fun(fun_def) => &fun_def.ret_ty,
            Def::Var(var_def) => &var_def.ty,
        }
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
}

impl Typed for Binary {
    fn get_type(&self) -> &ty::Type {
        &self.ty
    }
}

#[derive(Debug)]
pub struct Unary {
    pub id: ast::NodeId,
    pub op: ast::UnaryOpKind,
    pub expr: Box<Expr>,
}

impl Unary {
    pub fn new(id: ast::NodeId, op: ast::UnaryOpKind, expr: Expr) -> Self {
        Unary {
            id,
            op,
            expr: Box::new(expr),
        }
    }
}

impl Typed for Unary {
    fn get_type(&self) -> &ty::Type {
        &self.expr.get_type()
    }
}
