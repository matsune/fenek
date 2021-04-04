use crate::mir;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct ScopeTable {
    inner: HashMap<String, Def>,
}

impl ScopeTable {
    pub fn lookup(&self, symbol: &str) -> Option<&Def> {
        self.inner.get(symbol)
    }

    pub fn lookup_mut(&mut self, symbol: &str) -> Option<&mut Def> {
        self.inner.get_mut(symbol)
    }

    pub fn insert_var(&mut self, symbol: String, def: VarDef) {
        self.inner.insert(symbol, Def::Var(def));
    }
}

pub type DefId = usize;

#[derive(Debug, Clone)]
pub enum Def {
    Fun(FunDef),
    Var(VarDef),
}

impl Def {
    pub fn into_fun_def(self) -> FunDef {
        match self {
            Def::Fun(def) => def,
            _ => panic!(),
        }
    }

    pub fn into_var_def(self) -> VarDef {
        match self {
            Def::Var(var_def) => var_def,
            _ => panic!(),
        }
    }

    pub fn as_var_def(&self) -> &VarDef {
        match self {
            Def::Var(ref var_def) => var_def,
            _ => panic!(),
        }
    }

    pub fn get_type(&self) -> mir::Type {
        match self {
            Def::Fun(fun_def) => fun_def.ret_ty,
            Def::Var(var_def) => var_def.ty,
        }
    }

    pub fn id(&self) -> DefId {
        match self {
            Def::Fun(fun_def) => fun_def.id,
            Def::Var(var_def) => var_def.id,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunDef {
    pub id: DefId,
    pub ret_ty: mir::Type,
    pub arg_tys: Vec<mir::Type>,
}

impl FunDef {
    pub fn new(id: DefId, ret_ty: mir::Type, arg_tys: Vec<mir::Type>) -> Self {
        Self {
            id,
            ret_ty,
            arg_tys,
        }
    }
}

impl Into<Def> for FunDef {
    fn into(self) -> Def {
        Def::Fun(self)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct VarDef {
    pub id: DefId,
    pub ty: mir::Type,
    pub is_mut: bool,
    pub is_arg: bool,
}

impl VarDef {
    pub fn new(id: DefId, ty: mir::Type, is_mut: bool, is_arg: bool) -> Self {
        Self {
            id,
            ty,
            is_mut,
            is_arg,
        }
    }
}

impl Into<Def> for VarDef {
    fn into(self) -> Def {
        Def::Var(self)
    }
}
