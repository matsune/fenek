use hir::def::{Def, DefFun, DefStruct, DefVar};
use std::collections::HashMap;
use types::infer::InferTy;

pub type ArenaIdx = usize;

pub struct Scopes<'lower> {
    scopes: Vec<ScopeTable<'lower>>,
    scope_idx: ArenaIdx,
}

impl<'lower> Default for Scopes<'lower> {
    fn default() -> Self {
        let mut scopes = Vec::new();
        let scope_idx = scopes.len();
        // add global scope
        scopes.push(ScopeTable::new(scope_idx, None));
        Self { scopes, scope_idx }
    }
}

impl<'lower> Scopes<'lower> {
    pub fn push_scope(&mut self) {
        let idx = self.scopes.len();
        self.scopes.push(ScopeTable::new(idx, Some(self.scope_idx)));
        self.scope_idx = idx;
    }

    pub fn pop_scope(&mut self) {
        self.scope_idx = self.scopes[self.scope_idx].parent.unwrap();
    }

    pub fn insert(&mut self, key: String, value: &'lower Def<&'lower InferTy<'lower>>) {
        self.scopes[self.scope_idx].insert(key, value)
    }

    pub fn lookup_fun(
        &self,
        name: &str,
        only_this: bool,
    ) -> Option<&'lower DefFun<&'lower InferTy<'lower>>> {
        if only_this {
            return self.scopes[self.scope_idx].lookup_fun(name);
        }
        self.iterate_scope(|scope_table| scope_table.lookup_fun(name))
    }

    pub fn lookup_var(
        &self,
        name: &str,
        only_this: bool,
    ) -> Option<&'lower DefVar<&'lower InferTy<'lower>>> {
        if only_this {
            return self.scopes[self.scope_idx].lookup_var(name);
        }
        self.iterate_scope(|scope_table| scope_table.lookup_var(name))
    }

    // pub fn lookup_struct(
    //     &self,
    //     name: &str,
    //     only_this: bool,
    // ) -> Option<&'lower DefStruct<&'lower InferTy<'lower>>> {
    //     if only_this {
    //         return self.scopes[self.scope_idx].lookup_struct(name);
    //     }
    //     self.iterate_scope(|scope_table| scope_table.lookup_struct(name))
    // }

    fn iterate_scope<T, F>(&self, f: F) -> Option<T>
    where
        F: Fn(&ScopeTable<'lower>) -> Option<T>,
    {
        let scopes = &self.scopes;
        let mut tmp_idx = self.scope_idx;
        while let Some(scope_table) = scopes.get(tmp_idx) {
            match f(scope_table) {
                Some(t) => return Some(t),
                None => match scope_table.parent {
                    Some(parent_idx) => tmp_idx = parent_idx,
                    None => break,
                },
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct ScopeTable<'lower> {
    pub idx: ArenaIdx,
    pub table: HashMap<String, &'lower Def<&'lower InferTy<'lower>>>,
    pub parent: Option<ArenaIdx>,
}

impl<'lower> ScopeTable<'lower> {
    pub fn new(idx: ArenaIdx, parent: Option<ArenaIdx>) -> Self {
        Self {
            idx,
            table: HashMap::new(),
            parent,
        }
    }

    pub fn insert(&mut self, key: String, value: &'lower Def<&'lower InferTy<'lower>>) {
        self.table.insert(key, value);
    }

    pub fn lookup_fun(&self, symbol: &str) -> Option<&'lower DefFun<&'lower InferTy<'lower>>> {
        match self.table.get(symbol) {
            Some(Def::Fun(inner)) => Some(inner),
            _ => None,
        }
    }

    pub fn lookup_var(&self, symbol: &str) -> Option<&'lower DefVar<&'lower InferTy<'lower>>> {
        match self.table.get(symbol) {
            Some(Def::Var(inner)) => Some(inner),
            _ => None,
        }
    }

    // pub fn lookup_struct(
    //     &self,
    //     symbol: &str,
    // ) -> Option<&'lower DefStruct<&'lower InferTy<'lower>>> {
    //     match self.table.get(symbol) {
    //         Some(Def::Struct(inner)) => Some(inner),
    //         _ => None,
    //     }
    // }
}
