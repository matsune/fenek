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

    pub fn bind(&self, a: &'a InferTy<'a>, b: &'a InferTy<'a>) -> Result<(), TypeError> {
        let a = a.prune();
        let b = b.prune();
        if a.id == b.id {
            return Ok(());
        }

        use InferTyKind::*;
        let unified_ty = match (&a.kind, &b.kind) {
            // Var
            (Var, _) => b,
            (_, Var) => a,

            // IntLit
            (IntLit, Int(_)) | (IntLit, IntLit) | (IntLit, Float(_)) | (IntLit, FloatLit) => b,
            (Int(_), IntLit) | (Float(_), IntLit) | (FloatLit, IntLit) => a,

            // FloatLit
            (FloatLit, FloatLit) | (FloatLit, Float(_)) => b,
            (Float(_), FloatLit) => a,

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

            // Ptr
            (Ptr(a), Ptr(b)) => {
                let ty = self.arena.alloc_var();
                self.bind(ty, a)?;
                self.bind(ty, b)?;
                self.arena.alloc_ptr(ty)
            }

            (a_kind, b_kind) => {
                if a_kind == b_kind {
                    a
                } else {
                    return Err(TypeError::ConflictTypes(
                        a_kind.to_string(),
                        b_kind.to_string(),
                    ));
                }
            }
        };

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
        let final_kind = &self.unify_tree(pruned_ty)?.kind;
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
            InferTyKind::Ptr(ty) => Type::Ptr(Box::new(self.solve_type(&ty)?)),
        };
        Ok(final_ty)
    }

    // recursively unify all children types from any point of tree
    fn unify_tree(&self, ty: &'a InferTy<'a>) -> Result<&'a InferTy<'a>, TypeError> {
        let mut ty = ty;
        for r_ty in ty.borrow_prevs().iter() {
            ty = self.unify(ty, self.unify_tree(r_ty)?)?;
        }
        Ok(ty)
    }

    fn unify(&self, a: &'a InferTy<'a>, b: &'a InferTy<'a>) -> Result<&'a InferTy<'a>, TypeError> {
        use InferTyKind::*;
        match (&a.kind, &b.kind) {
            // Var
            (Var, _) => Ok(b),
            (_, Var) => self.unify(b, a),

            // IntLit
            (IntLit, Int(_)) | (IntLit, IntLit) | (IntLit, Float(_)) | (IntLit, FloatLit) => Ok(b),
            (_, IntLit) => self.unify(b, a),

            // FloatLit
            (FloatLit, FloatLit) | (FloatLit, Float(_)) => Ok(b),
            (_, FloatLit) => self.unify(b, a),

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
                    arg_tys.push(self.unify(l_ty, r_ty)?);
                }
                let ret_ty = self.unify(l.ret_ty, r.ret_ty)?;
                Ok(self.arena.alloc_fun(arg_tys, ret_ty))
            }

            (Ptr(a), Ptr(b)) => {
                let t = self.unify(a, b)?;
                Ok(self.arena.alloc_ptr(t))
            }

            (a_kind, b_kind) => {
                if a_kind == b_kind {
                    Ok(a)
                } else {
                    Err(TypeError::ConflictTypes(
                        a_kind.to_string(),
                        b_kind.to_string(),
                    ))
                }
            }
        }
    }
}
