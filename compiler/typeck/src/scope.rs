use crate::mir;
use crate::tree::*;
use std::collections::HashMap;

// #[derive(Debug, Default)]
// pub struct ScopeTree {
//     inner: ArenaTree<Scope>,
// }

// impl ScopeTree {
//     pub fn add_scope(&mut self, scope: Scope) -> TreeNodeIdx {
//         self.inner.add_node(scope)
//     }

//     pub fn global_scope(&self) -> &TreeNode<Scope> {
//         self.inner.get(0).unwrap()
//     }

//     pub fn global_scope_mut(&mut self) -> &mut TreeNode<Scope> {
//         self.inner.get_mut(0).unwrap()
//     }

//     pub fn get(&self, idx: TreeNodeIdx) -> Option<&TreeNode<Scope>> {
//         self.inner.get(idx)
//     }

//     pub fn get_mut(&mut self, idx: TreeNodeIdx) -> Option<&mut TreeNode<Scope>> {
//         self.inner.get_mut(idx)
//     }
// }

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

#[derive(Debug, Clone, Copy)]
pub enum Def {
    Var(VarDef),
}

impl Def {
    pub fn into_var_def(self) -> VarDef {
        match self {
            Def::Var(var_def) => var_def,
        }
    }

    pub fn get_type(&self) -> mir::Type {
        match self {
            Def::Var(var_def) => var_def.ty,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct VarDef {
    pub ty: mir::Type,
    pub is_mut: bool,
}

impl VarDef {
    pub fn new(ty: mir::Type, is_mut: bool) -> Self {
        Self { ty, is_mut }
    }
}
