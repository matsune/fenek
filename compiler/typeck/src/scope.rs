use hir::def::{Def, DefFun, DefVar};
use std::collections::HashMap;
use types::infer::InferTy;

pub type ArenaIdx = usize;

pub struct Scopes<'infer> {
    scopes: Vec<ScopeTable<'infer>>,
    scope_idx: ArenaIdx,
}

impl<'infer> Default for Scopes<'infer> {
    fn default() -> Self {
        let mut scopes = Vec::new();
        let scope_idx = scopes.len();
        // add global scope
        scopes.push(ScopeTable::new(scope_idx, None));
        Self { scopes, scope_idx }
    }
}

impl<'infer> Scopes<'infer> {
    pub fn push_scope(&mut self) {
        let idx = self.scopes.len();
        self.scopes.push(ScopeTable::new(idx, Some(self.scope_idx)));
        self.scope_idx = idx;
    }

    pub fn pop_scope(&mut self) {
        self.scope_idx = self.scopes[self.scope_idx].parent.unwrap();
    }

    pub fn insert(&mut self, key: String, value: &'infer Def<&'infer InferTy<'infer>>) {
        self.scopes[self.scope_idx].insert(key, value)
    }

    pub fn lookup_fun(
        &self,
        name: &str,
        only_this: bool,
    ) -> Option<&'infer DefFun<&'infer InferTy<'infer>>> {
        if only_this {
            return self.scopes[self.scope_idx].lookup_fun(name);
        }
        self.iterate_scope(|scope_table| scope_table.lookup_fun(name))
    }

    pub fn lookup_var(
        &self,
        name: &str,
        only_this: bool,
    ) -> Option<&'infer DefVar<&'infer InferTy<'infer>>> {
        if only_this {
            return self.scopes[self.scope_idx].lookup_var(name);
        }
        self.iterate_scope(|scope_table| scope_table.lookup_var(name))
    }

    fn iterate_scope<T, F>(&self, f: F) -> Option<T>
    where
        F: Fn(&ScopeTable<'infer>) -> Option<T>,
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
pub struct ScopeTable<'infer> {
    pub idx: ArenaIdx,
    pub table: HashMap<String, &'infer Def<&'infer InferTy<'infer>>>,
    pub parent: Option<ArenaIdx>,
}

impl<'infer> ScopeTable<'infer> {
    pub fn new(idx: ArenaIdx, parent: Option<ArenaIdx>) -> Self {
        Self {
            idx,
            table: HashMap::new(),
            parent,
        }
    }

    pub fn insert(&mut self, key: String, value: &'infer Def<&'infer InferTy<'infer>>) {
        self.table.insert(key, value);
    }

    pub fn lookup_fun(&self, symbol: &str) -> Option<&'infer DefFun<&'infer InferTy<'infer>>> {
        match self.table.get(symbol) {
            Some(Def::Fun(inner)) => Some(inner),
            _ => None,
        }
    }

    pub fn lookup_var(&self, symbol: &str) -> Option<&'infer DefVar<&'infer InferTy<'infer>>> {
        match self.table.get(symbol) {
            Some(Def::Var(inner)) => Some(inner),
            _ => None,
        }
    }
}
