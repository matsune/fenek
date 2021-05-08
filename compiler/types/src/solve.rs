use super::infer::*;
use super::ty::*;
use error::TypeError;

#[cfg(test)]
mod tests;

pub struct Solver<'a> {
    pub arena: &'a InferTyArena<'a>,
}

impl<'a> Solver<'a> {
    pub fn new(arena: &'a InferTyArena<'a>) -> Self {
        Self { arena }
    }

    fn bind_unify(
        &self,
        a: &'a InferTy<'a>,
        b: &'a InferTy<'a>,
    ) -> Result<&'a InferTy<'a>, TypeError> {
        use InferTyKind::*;
        let ty = match (&a.kind, &b.kind) {
            // Var
            (Var, _) => b,
            (_, Var) => a,

            // IntLit
            (IntLit, Int(_)) | (IntLit, IntLit) | (IntLit, Float(_)) | (IntLit, FloatLit) => b,
            (Int(_), IntLit) | (Float(_), IntLit) | (FloatLit, IntLit) => a,

            // FloatLit
            (FloatLit, FloatLit) | (FloatLit, Float(_)) => b,
            (Float(_), FloatLit) => a,

            (Int(at), Int(bt)) if at == bt => a,
            (Float(at), Float(bt)) if at == bt => a,
            (Bool, Bool) | (Void, Void) => a,

            // Fun
            (Fun(l), Fun(r)) => {
                if l.arg_tys.len() != r.arg_tys.len() {
                    return Err(TypeError::ConflictTypes(
                        a.kind.to_string(),
                        b.kind.to_string(),
                    ));
                }
                let mut arg_tys = Vec::with_capacity(l.arg_tys.len());
                for i in 0..l.arg_tys.len() {
                    let arg_ty = self.arena.alloc_var();
                    let l_ty = l.arg_tys[i];
                    let r_ty = l.arg_tys[i];
                    self.bind(arg_ty, l_ty)?;
                    self.bind(arg_ty, r_ty)?;
                    arg_tys.push(arg_ty);
                }
                let ret_ty = self.arena.alloc_var();
                self.bind(ret_ty, l.ret_ty)?;
                self.bind(ret_ty, r.ret_ty)?;
                self.arena.alloc_fun(arg_tys, ret_ty)
            }

            // Ref
            (Ref(a), Ref(b)) => self.arena.alloc_ref(self.bind_unify(a, b)?),

            _ => {
                return Err(TypeError::ConflictTypes(
                    a.kind.to_string(),
                    b.kind.to_string(),
                ))
            }
        };
        Ok(ty)
    }

    pub fn bind(&self, a: &'a InferTy<'a>, b: &'a InferTy<'a>) -> Result<(), TypeError> {
        let a = a.prune();
        let b = b.prune();
        if a.id == b.id {
            return Ok(());
        }

        let unified_ty = self.bind_unify(a, b)?;

        if unified_ty.id != a.id {
            a.next.set(Some(unified_ty));
            unified_ty.prevs.borrow_mut().push(a);
        }
        if unified_ty.id != b.id {
            b.next.set(Some(unified_ty));
            unified_ty.prevs.borrow_mut().push(b);
        }
        Ok(())
    }

    pub fn solve_type(&self, ty: &'a InferTy<'a>) -> Result<Type, TypeError> {
        let pruned_ty = ty.prune();
        let final_kind = &self.solve_tree(pruned_ty)?.kind;
        let final_ty = match final_kind {
            InferTyKind::Var => {
                return Err(TypeError::UnresolvedType);
            }
            InferTyKind::Int(int_kind) => match int_kind {
                IntKind::I8 => Type::Int(IntType::I8),
                IntKind::I16 => Type::Int(IntType::I16),
                IntKind::I32 => Type::Int(IntType::I32),
                IntKind::I64 => Type::Int(IntType::I64),
            },
            // FIXME: architecture bit width
            InferTyKind::IntLit => Type::Int(IntType::I64),
            InferTyKind::Float(kind) => match kind {
                FloatKind::F32 => Type::Float(FloatType::F32),
                FloatKind::F64 => Type::Float(FloatType::F64),
            },
            InferTyKind::FloatLit => Type::Float(FloatType::F64),
            InferTyKind::Bool => Type::Bool,
            InferTyKind::Void => Type::Void,
            InferTyKind::Fun(fun_ty) => {
                let mut arg_tys = Vec::new();
                for arg_ty in &fun_ty.arg_tys {
                    arg_tys.push(self.solve_type(arg_ty)?);
                }
                let ret_ty = self.solve_type(&fun_ty.ret_ty)?;
                Type::Fun(FunType::new(arg_tys, Box::new(ret_ty)))
            }
            InferTyKind::Ref(ty) => Type::Ref(Box::new(self.solve_type(&ty)?)),
        };
        Ok(final_ty)
    }

    // recursively unify all children types from any point of tree
    fn solve_tree(&self, ty: &'a InferTy<'a>) -> Result<&'a InferTy<'a>, TypeError> {
        let mut ty = ty;
        for r_ty in ty.borrow_prevs().iter() {
            ty = self.solve_unify(ty, self.solve_tree(r_ty)?)?;
        }
        Ok(ty)
    }

    fn solve_unify(
        &self,
        a: &'a InferTy<'a>,
        b: &'a InferTy<'a>,
    ) -> Result<&'a InferTy<'a>, TypeError> {
        use InferTyKind::*;
        let ty = match (&a.kind, &b.kind) {
            // Var
            (Var, _) => b,
            (_, Var) => self.solve_unify(b, a)?,

            // IntLit
            (IntLit, Int(_)) | (IntLit, IntLit) | (IntLit, Float(_)) | (IntLit, FloatLit) => b,
            (_, IntLit) => self.solve_unify(b, a)?,

            // FloatLit
            (FloatLit, FloatLit) | (FloatLit, Float(_)) => b,
            (_, FloatLit) => self.solve_unify(b, a)?,

            (Int(at), Int(bt)) if at == bt => a,
            (Float(at), Float(bt)) if at == bt => a,
            (Bool, Bool) | (Void, Void) => a,

            // Fun
            (Fun(l), Fun(r)) => {
                if l.arg_tys.len() != r.arg_tys.len() {
                    return Err(TypeError::ConflictTypes(
                        a.kind.to_string(),
                        b.kind.to_string(),
                    ));
                }
                let mut arg_tys = Vec::with_capacity(l.arg_tys.len());
                for i in 0..l.arg_tys.len() {
                    let l_ty = l.arg_tys[i];
                    let r_ty = l.arg_tys[i];
                    arg_tys.push(self.solve_unify(l_ty, r_ty)?);
                }
                let ret_ty = self.solve_unify(l.ret_ty, r.ret_ty)?;
                self.arena.alloc_fun(arg_tys, ret_ty)
            }

            (Ref(a), Ref(b)) => self.arena.alloc_ref(self.solve_unify(a, b)?),

            _ => {
                return Err(TypeError::ConflictTypes(
                    a.kind.to_string(),
                    b.kind.to_string(),
                ))
            }
        };
        Ok(ty)
    }
}
