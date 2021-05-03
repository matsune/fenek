use super::infer::*;
use error::{Result, TypeCkError};
use hir::ty;

#[cfg(test)]
mod tests;

pub struct TyFinalizer<'a> {
    arena: &'a InferTyArena<'a>,
}

impl<'a> TyFinalizer<'a> {
    pub fn new(arena: &'a InferTyArena<'a>) -> Self {
        Self { arena }
    }

    pub fn finalize_type(&self, ty: &'a InferTy<'a>) -> Result<ty::Type, TypeCkError> {
        let top_ty = ty.prune();
        let final_kind = &self.unify_tree(top_ty)?.kind;
        let final_ty = match final_kind {
            InferTyKind::Var => {
                return Err(TypeCkError::UnresolvedType);
            }
            InferTyKind::Int(int_kind) => match int_kind {
                IntKind::I8 => ty::Type::Int(ty::IntType::I8),
                IntKind::I16 => ty::Type::Int(ty::IntType::I16),
                IntKind::I32 => ty::Type::Int(ty::IntType::I32),
                IntKind::I64 => ty::Type::Int(ty::IntType::I64),
            },
            // FIXME: architecture bit width
            InferTyKind::IntLit => ty::Type::Int(ty::IntType::I64),
            InferTyKind::Float(kind) => match kind {
                FloatKind::F32 => ty::Type::Float(ty::FloatType::F32),
                FloatKind::F64 => ty::Type::Float(ty::FloatType::F64),
            },
            InferTyKind::FloatLit => ty::Type::Float(ty::FloatType::F64),
            InferTyKind::Bool => ty::Type::Bool,
            InferTyKind::Void => ty::Type::Void,
            InferTyKind::Fun(fun_ty) => {
                let mut arg_tys = Vec::new();
                for arg_ty in &fun_ty.arg_tys {
                    arg_tys.push(self.finalize_type(arg_ty)?);
                }
                let ret_ty = self.finalize_type(&fun_ty.ret_ty)?;
                ty::Type::Fun(ty::FunType::new(arg_tys, Box::new(ret_ty)))
            }
        };
        Ok(final_ty)
    }

    // recursively unify all children types from any point of tree
    fn unify_tree(&self, ty: &'a InferTy<'a>) -> Result<&'a InferTy<'a>, TypeCkError> {
        let mut ty = ty;
        for r_ty in ty.borrow_from_nodes().iter() {
            ty = self.unify(ty, self.unify_tree(r_ty)?)?;
        }
        Ok(ty)
    }

    fn unify(
        &self,
        a: &'a InferTy<'a>,
        b: &'a InferTy<'a>,
    ) -> Result<&'a InferTy<'a>, TypeCkError> {
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
                    return Err(TypeCkError::ConflictTypes(
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

            (a_kind, b_kind) => {
                if a_kind == b_kind {
                    Ok(a)
                } else {
                    Err(TypeCkError::ConflictTypes(
                        a_kind.to_string(),
                        b_kind.to_string(),
                    ))
                }
            }
        }
    }
}
