use hir::def::{Def, DefKind};
use typed_arena::Arena;

pub struct DefArena<Ty> {
    pub inner: Arena<Def<Ty>>,
}

impl<Ty> DefArena<Ty> {
    pub fn new() -> Self {
        Self {
            inner: Arena::new(),
        }
    }

    pub fn alloc(&self, ty: Ty, kind: DefKind) -> &Def<Ty> {
        self.inner.alloc(Def::new(self.inner.len(), ty, kind))
    }
}
