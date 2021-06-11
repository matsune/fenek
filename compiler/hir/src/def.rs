use serde::Serialize;

pub type DefId = usize;

/// User defined symbols. This will be created when
/// user defines functions or variables which has a
/// unique name in the scope.
///
/// Generic type <Ty> will be `infer_ty::InferTy` or
/// `ty::Type` because this will be used by either
/// type inference and type checking.
#[derive(Debug, Clone, Serialize)]
pub enum Def<Ty> {
    Var(DefVar<Ty>),
    Fun(DefFun<Ty>),
}

impl<Ty> Def<Ty> {
    pub fn id(&self) -> DefId {
        match self {
            Self::Fun(inner) => inner.id,
            Self::Var(inner) => inner.id,
        }
    }

    pub fn ty(&self) -> &Ty {
        match self {
            Self::Fun(inner) => &inner.ty,
            Self::Var(inner) => &inner.ty,
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Def::Var(_))
    }

    pub fn is_fun(&self) -> bool {
        matches!(self, Def::Fun(_))
    }

    pub fn is_mutable(&self) -> bool {
        matches!(&self, Def::Var(var) if var.is_mut)
    }

    pub fn as_fun(&self) -> &DefFun<Ty> {
        match &self {
            Def::Fun(def_fun) => def_fun,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct DefVar<Ty> {
    pub id: DefId,
    pub ty: Ty,
    pub is_mut: bool,
}

impl<Ty> Into<Def<Ty>> for DefVar<Ty> {
    fn into(self) -> Def<Ty> {
        Def::Var(self)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct DefFun<Ty> {
    pub id: DefId,
    pub ty: Ty,
    pub arg_muts: Vec<bool>,
    pub ret_mut: bool,
}

impl<Ty> Into<Def<Ty>> for DefFun<Ty> {
    fn into(self) -> Def<Ty> {
        Def::Fun(self)
    }
}
