use super::ty;
use serde::Serialize;
use std::ops::Deref;

#[derive(Serialize)]
pub struct Module {
    funs: Vec<Fun>,
}

impl From<&hir::Module> for Module {
    fn from(base: &hir::Module) -> Self {
        Self {
            funs: base.funs.iter().map(|fun| fun.into()).collect(),
        }
    }
}

#[derive(Serialize)]
pub struct Fun {
    id: ast::NodeId,
    def_id: hir::def::DefId,
    ty: ty::Type,
    name: String,
    args: FunArgs,
    block: Block,
}

impl From<&hir::Fun> for Fun {
    fn from(base: &hir::Fun) -> Self {
        Self {
            id: base.id,
            def_id: base.def.id,
            ty: (&base.def.ty).into(),
            name: base.name.clone(),
            args: base.args.iter().map(|ident| ident.raw.clone()).collect(),
            // ret_ty: base.ret_ty.deref().into(),
            block: (&base.block).into(),
        }
    }
}

type FunArgs = Vec<String>;

#[derive(Serialize)]
struct Block {
    id: ast::NodeId,
    stmts: Vec<Stmt>,
}

impl From<&hir::Block> for Block {
    fn from(base: &hir::Block) -> Self {
        Block {
            id: base.id,
            stmts: base.stmts.iter().map(|stmt| stmt.into()).collect(),
        }
    }
}

macro_rules! Enum {
    ($name:ident [$($Var:ident),*]) => {
        #[derive(Serialize)]
        enum $name {
            $(
                $Var($Var),
            )*
        }

        impl From<&hir::$name> for $name {
            fn from(base: &hir::$name) -> Self {
                match base {
                $(
                    hir::$name::$Var(v) => $name::$Var(v.into()),
                )*
                }
            }
        }
    }
}

Enum!(Stmt [VarDecl, Ret, Expr]);

#[derive(Serialize)]
struct VarDecl {
    id: ast::NodeId,
    def_id: hir::def::DefId,
    ty: ty::Type,
    name: String,
    init: Box<Expr>,
}

impl From<&hir::VarDecl> for VarDecl {
    fn from(base: &hir::VarDecl) -> Self {
        Self {
            id: base.id,
            def_id: base.def.id,
            ty: (&base.def.ty).into(),
            name: base.name.raw.clone(),
            init: Box::new(base.init.deref().into()),
        }
    }
}

#[derive(Serialize)]
struct Ret {
    id: ast::NodeId,
    expr: Option<Expr>,
}

impl From<&hir::Ret> for Ret {
    fn from(base: &hir::Ret) -> Self {
        Ret {
            id: base.id,
            expr: base.expr.as_ref().map(|expr| expr.into()),
        }
    }
}

Enum!(Expr [Lit, Path,Call, Binary, Unary]);

#[derive(Serialize)]
struct Lit {
    id: ast::NodeId,
    kind: LitKind,
    ty: ty::Type,
}

impl From<&hir::Lit> for Lit {
    fn from(base: &hir::Lit) -> Self {
        Self {
            id: base.id,
            kind: (&base.kind).into(),
            ty: (&base.ty).into(),
        }
    }
}

#[derive(Serialize)]
enum LitKind {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
}

impl From<&hir::LitKind> for LitKind {
    fn from(base: &hir::LitKind) -> Self {
        match *base {
            hir::LitKind::I8(v) => Self::I8(v),
            hir::LitKind::I16(v) => Self::I16(v),
            hir::LitKind::I32(v) => Self::I32(v),
            hir::LitKind::I64(v) => Self::I64(v),
            hir::LitKind::F32(v) => Self::F32(v),
            hir::LitKind::F64(v) => Self::F64(v),
            hir::LitKind::Bool(v) => Self::Bool(v),
        }
    }
}

#[derive(Serialize)]
struct Path {
    raw: String,
    def_id: hir::def::DefId,
    ty: ty::Type,
}

impl From<&hir::Path> for Path {
    fn from(base: &hir::Path) -> Self {
        Self {
            raw: base.raw.clone(),
            def_id: base.def.id,
            ty: ty::Type::from(&base.def.ty),
            // match &base.def {
            // hir::def::Def::Fun(fun) => ty::Type::from(&hir::ty::Type::Fun(hir::ty::FunType {
            //     args: fun.arg_tys.clone(),
            //     ret: Some(Box::new(fun.ret_ty.clone())),
            // })),
            // hir::def::Def::Var(var) => (&var.ty).into(),
            // },
        }
    }
}

#[derive(Serialize)]
struct Call {
    path: String,
    args: Vec<Expr>,
    def_id: hir::def::DefId,
    ty: ty::Type,
}

impl From<&hir::Call> for Call {
    fn from(base: &hir::Call) -> Self {
        Self {
            path: base.path.clone(),
            args: base.args.iter().map(|arg| arg.into()).collect(),
            def_id: base.def.id,
            ty: base.def.ty.as_fun().ret.deref().into(),
        }
    }
}

#[derive(Serialize)]
struct Binary {
    id: ast::NodeId,
    op: crate::ast::BinOpKind,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    ty: ty::Type,
}

impl From<&hir::Binary> for Binary {
    fn from(base: &hir::Binary) -> Self {
        Self {
            id: base.id,
            op: base.op.into(),
            lhs: Box::new(base.lhs.deref().into()),
            rhs: Box::new(base.rhs.deref().into()),
            ty: (&base.ty).into(),
        }
    }
}

#[derive(Serialize)]
struct Unary {
    id: ast::NodeId,
    op: crate::ast::UnaryOpKind,
    expr: Box<Expr>,
}

impl From<&hir::Unary> for Unary {
    fn from(base: &hir::Unary) -> Self {
        Self {
            id: base.id,
            op: base.op.into(),
            expr: Box::new(base.expr.deref().into()),
        }
    }
}
