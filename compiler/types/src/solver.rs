use super::infer::{InferTy, InferTyArena};
use super::result::Result;
use super::ty::*;
use error::TypeError;
use typed_arena::Arena;

#[cfg(test)]
mod tests;

pub struct Solver<'a> {
    pub ty_arena: &'a InferTyArena<'a>,
    pub struct_arena: &'a Arena<StructType>,
}

impl<'a> Solver<'a> {
    pub fn new(ty_arena: &'a InferTyArena<'a>, struct_arena: &'a Arena<StructType>) -> Self {
        Self {
            ty_arena,
            struct_arena,
        }
    }

    pub fn add_struct<S: ToString>(&self, name: S) -> &'a StructType {
        let id = self.struct_arena.len();
        self.struct_arena.alloc(StructType::new(id, name))
    }

    pub fn infer_ty_from_type(&self, ty: Type) -> &'a InferTy<'a> {
        match ty {
            Type::Int(ty) => match ty {
                IntType::I8 => self.ty_arena.alloc_i8(),
                IntType::I16 => self.ty_arena.alloc_i16(),
                IntType::I32 => self.ty_arena.alloc_i32(),
                IntType::I64 => self.ty_arena.alloc_i64(),
            },
            Type::Float(ty) => match ty {
                FloatType::F32 => self.ty_arena.alloc_f32(),
                FloatType::F64 => self.ty_arena.alloc_f64(),
            },
            Type::Bool => self.ty_arena.alloc_bool(),
            Type::Ptr(elem) => self.ty_arena.alloc_ptr(self.infer_ty_from_type(*elem)),
            Type::Void => self.ty_arena.alloc_void(),
            Type::Fun(fun_ty) => {
                let args = fun_ty
                    .args
                    .iter()
                    .map(|ty| self.infer_ty_from_type(ty.clone()))
                    .collect::<Vec<_>>();
                let ret = self.infer_ty_from_type(*fun_ty.ret);
                self.ty_arena.alloc_fun(args, ret)
            }
            Type::Struct(struct_ty) => self.ty_arena.alloc_struct(struct_ty),
        }
    }

    /// get the most tip node from this node.
    pub fn prune(&self, ty: &'a InferTy<'a>) -> &'a InferTy<'a> {
        match ty {
            InferTy::Any(bindable) | InferTy::IntLit(bindable) | InferTy::FloatLit(bindable) => {
                bindable
                    .next
                    .get()
                    .map(|next| self.prune(next))
                    .unwrap_or(ty)
            }
            InferTy::Ptr(elem) => self.ty_arena.alloc_ptr(self.prune(elem)),
            InferTy::Fun(fun) => {
                let mut args = Vec::with_capacity(fun.args.len());
                for arg in fun.args.iter() {
                    args.push(self.prune(arg));
                }
                let ret = self.prune(fun.ret);
                self.ty_arena.alloc_fun(args, ret)
            }
            _ => ty,
        }
    }

    pub fn bind(&self, a: &'a InferTy<'a>, b: &'a InferTy<'a>) -> Result<&'a InferTy<'a>> {
        let a = self.prune(a);
        let b = self.prune(b);
        if a == b {
            return Ok(a);
        }

        let unified_ty = match (a, b) {
            // Any
            (InferTy::Any(_), _) => b,
            (_, InferTy::Any(_)) => a,

            // IntLit - *
            (InferTy::IntLit(_), InferTy::IntLit(_))
            | (InferTy::IntLit(_), InferTy::FloatLit(_))
            | (InferTy::IntLit(_), InferTy::Int(_))
            | (InferTy::IntLit(_), InferTy::Float(_)) => b,
            // * - IntLit
            (InferTy::FloatLit(_), InferTy::IntLit(_))
            | (InferTy::Int(_), InferTy::IntLit(_))
            | (InferTy::Float(_), InferTy::IntLit(_)) => a,

            // FloatLit - *
            (InferTy::FloatLit(_), InferTy::FloatLit(_))
            | (InferTy::FloatLit(_), InferTy::Float(_)) => b,
            // * - FloatLit
            (InferTy::Float(_), InferTy::FloatLit(_)) => a,

            (InferTy::Ptr(elem_a), InferTy::Ptr(elem_b)) => {
                let elem = self.bind(elem_a, elem_b)?;
                self.ty_arena.alloc_ptr(elem)
            }

            (InferTy::Fun(fun_a), InferTy::Fun(fun_b)) => {
                if fun_a.args.len() != fun_b.args.len() {
                    return Err(TypeError::ConflictTypes(a.to_string(), b.to_string()));
                }
                let mut arg_tys = Vec::with_capacity(fun_a.args.len());
                for i in 0..fun_a.args.len() {
                    let arg_a = fun_a.args[i];
                    let arg_b = fun_b.args[i];
                    arg_tys.push(self.bind(arg_a, arg_b)?);
                }
                let ret_ty = self.bind(fun_a.ret, fun_b.ret)?;
                self.ty_arena.alloc_fun(arg_tys, ret_ty)
            }

            (InferTy::Struct(s_a), InferTy::Struct(s_b)) if s_a.id == s_b.id => a,

            _ => {
                return Err(TypeError::ConflictTypes(a.to_string(), b.to_string()));
            }
        };

        if a != unified_ty {
            if let Some(bindable) = a.try_as_bindable() {
                bindable.next.set(Some(unified_ty));
            }
        }
        if b != unified_ty {
            if let Some(bindable) = b.try_as_bindable() {
                bindable.next.set(Some(unified_ty));
            }
        }
        Ok(unified_ty)
    }

    pub fn solve_type(&self, ty: &'a InferTy<'a>) -> Result<Type> {
        let ty = self.prune(ty);
        match ty {
            InferTy::Any(_) => Err(TypeError::UnresolvedType),
            InferTy::IntLit(_) => Ok(Type::Int(IntType::I64)),
            InferTy::FloatLit(_) => Ok(Type::Float(FloatType::F64)),
            InferTy::Int(inner) => Ok(Type::Int(*inner)),
            InferTy::Float(inner) => Ok(Type::Float(*inner)),
            InferTy::Bool => Ok(Type::Bool),
            InferTy::Void => Ok(Type::Void),
            InferTy::Struct(strukt) => Ok(Type::Struct((*strukt).clone())),
            InferTy::Ptr(elem) => Ok(Type::Ptr(Box::new(self.solve_type(elem)?))),
            InferTy::Fun(fun) => {
                let mut args = Vec::with_capacity(fun.args.len());
                for arg in fun.args.iter() {
                    args.push(self.solve_type(arg)?);
                }
                let ret = self.solve_type(fun.ret)?;
                Ok(Type::Fun(FunType {
                    args,
                    ret: Box::new(ret),
                }))
            }
        }
    }
}
