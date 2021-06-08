use super::arena::InferTyArena;
use super::ty::*;
use super::Result;
use crate::ty::{FunType, Type};
use error::TypeError;

#[cfg(test)]
mod tests;

pub fn bind<'a>(
    arena: &'a InferTyArena<'a>,
    a: &'a InferTy<'a>,
    b: &'a InferTy<'a>,
) -> Result<&'a InferTy<'a>> {
    let a = a.prune();
    let b = b.prune();
    if a.id == b.id {
        return Ok(a);
    }

    let unified_ty = match (&a.kind, &b.kind) {
        // type variable `Var` can be anything
        (TyKind::Any, _) => b,
        (_, TyKind::Any) => a,

        (TyKind::Atom(atom_a), TyKind::Atom(atom_b)) => {
            if atom_a.id == atom_b.id {
                a
            } else if atom_a.family.contains(&atom_b.id) {
                b
            } else if atom_b.family.contains(&atom_a.id) {
                a
            } else {
                return Err(TypeError::ConflictTypes(
                    a.kind.to_string(),
                    b.kind.to_string(),
                ));
            }
        }

        (TyKind::Ptr(ptr_a), TyKind::Ptr(ptr_b)) => {
            let inner = bind(arena, ptr_a, ptr_b)?;
            arena.alloc_ptr(inner)
        }

        (TyKind::Fun(fun_a), TyKind::Fun(fun_b)) => {
            if fun_a.args.len() != fun_b.args.len() {
                return Err(TypeError::ConflictTypes(
                    a.kind.to_string(),
                    b.kind.to_string(),
                ));
            }
            let mut arg_tys = Vec::with_capacity(fun_a.args.len());
            for i in 0..fun_a.args.len() {
                let arg_a = fun_a.args[i];
                let arg_b = fun_b.args[i];
                arg_tys.push(bind(arena, arg_a, arg_b)?);
            }
            let ret_ty = bind(arena, fun_a.ret, fun_b.ret)?;
            arena.alloc_fun(arg_tys, ret_ty)
        }

        (a_kind, b_kind) => {
            return Err(TypeError::ConflictTypes(
                a_kind.to_string(),
                b_kind.to_string(),
            ));
        }
    };
    // associate types to make a same type tree
    a.set_prune(unified_ty);
    b.set_prune(unified_ty);
    Ok(unified_ty)
}

pub fn solve_type<'a>(ty: &'a InferTy<'a>) -> Result<Type> {
    let ty = ty.prune();
    match &ty.kind {
        TyKind::Any => Err(TypeError::UnresolvedType),
        TyKind::Atom(atom) => Ok(atom.solve_ty()),
        TyKind::Ptr(elem) => Ok(Type::Ptr(Box::new(solve_type(elem)?))),
        TyKind::Fun(fun) => {
            let mut args = Vec::with_capacity(fun.args.len());
            for arg in fun.args.iter() {
                args.push(solve_type(arg)?);
            }
            let ret = solve_type(fun.ret)?;
            Ok(Type::Fun(FunType::new(args, ret)))
        }
    }
}
