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

    /// Compare 2 types and bind them as same type if they are
    /// compatible infer types.
    pub fn bind(&self, a: &'a InferTy<'a>, b: &'a InferTy<'a>) -> Result<(), TypeError> {
        let a = a.prune();
        let b = b.prune();
        if a.id == b.id {
            return Ok(());
        }

        use InferTyKind::*;
        let unified_ty = match (&a.kind, &b.kind) {
            // type variable `Var` can be anything
            (Var, _) => b,
            (_, Var) => a,

            // int literal can be any number type
            (IntLit, Int(_)) | (IntLit, IntLit) | (IntLit, Float(_)) | (IntLit, FloatLit) => b,
            (Int(_), IntLit) | (Float(_), IntLit) | (FloatLit, IntLit) => a,

            // float literal can be any float type
            (FloatLit, FloatLit) | (FloatLit, Float(_)) => b,
            (Float(_), FloatLit) => a,

            // Fun can be same form Fun
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
                    self.bind(l_ty, r_ty)?;
                    arg_tys.push(l_ty);
                }
                self.bind(l.ret_ty, r.ret_ty)?;
                self.arena.alloc_fun(arg_tys, l.ret_ty)
            }

            // Ref can be Ref which has same element type
            (Ref(a), Ref(b)) => {
                self.bind(a, b)?;
                self.arena.alloc_ref(a)
            }

            (a_kind, b_kind) if a_kind == b_kind => a,

            (a_kind, b_kind) => {
                return Err(TypeError::ConflictTypes(
                    a_kind.to_string(),
                    b_kind.to_string(),
                ));
            }
        };

        // associate types to make type tree

        if unified_ty.id != a.id {
            a.next.set(Some(unified_ty));
        }
        if unified_ty.id != b.id {
            b.next.set(Some(unified_ty));
        }
        Ok(())
    }

    pub fn solve_type(&self, ty: &'a InferTy<'a>) -> Result<Type, TypeError> {
        let pruned_ty = ty.prune();
        let final_ty = match &pruned_ty.kind {
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
            InferTyKind::Ref(ty) => Type::Ptr(Box::new(self.solve_type(&ty)?)),
        };
        Ok(final_ty)
    }
}
