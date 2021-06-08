use super::ty::*;
use super::Result;
use crate::ty::{FloatType, IntType, Type};
use error::TypeError;
use std::collections::HashMap;
use typed_arena::Arena;

#[derive(Default)]
pub struct InferTyArena<'a> {
    pub type_pool: HashMap<String, AtomTyID>,
    pub inner: Arena<InferTy<'a>>,
}

impl<'a> InferTyArena<'a> {
    pub fn init(&mut self) -> Result<()> {
        self.register_type("void")?;
        self.register_type("i8")?;
        self.register_type("i16")?;
        self.register_type("i32")?;
        self.register_type("i64")?;
        self.register_type("f32")?;
        self.register_type("f64")?;
        self.register_type("int")?;
        self.register_type("float")?;
        Ok(())
    }

    pub fn get_id(&self, ty_name: &str) -> Option<AtomTyID> {
        self.type_pool.get(ty_name).copied()
    }

    pub fn alloc_i8(&'a self) -> &'a InferTy<'a> {
        self.alloc_atom("i8", Vec::new(), Type::Int(IntType::I8))
            .expect("not initialized")
    }

    pub fn alloc_i16(&'a self) -> &'a InferTy<'a> {
        self.alloc_atom("i16", Vec::new(), Type::Int(IntType::I16))
            .expect("not initialized")
    }

    pub fn alloc_i32(&'a self) -> &'a InferTy<'a> {
        self.alloc_atom("i32", Vec::new(), Type::Int(IntType::I32))
            .expect("not initialized")
    }

    pub fn alloc_i64(&'a self) -> &'a InferTy<'a> {
        self.alloc_atom("i64", Vec::new(), Type::Int(IntType::I64))
            .expect("not initialized")
    }

    pub fn alloc_f32(&'a self) -> &'a InferTy<'a> {
        self.alloc_atom("f32", Vec::new(), Type::Float(FloatType::F32))
            .expect("not initialized")
    }

    pub fn alloc_f64(&'a self) -> &'a InferTy<'a> {
        self.alloc_atom("f64", Vec::new(), Type::Float(FloatType::F64))
            .expect("not initialized")
    }

    pub fn alloc_int_lit(&'a self) -> &'a InferTy<'a> {
        self.alloc_atom(
            "int",
            Vec::from([
                self.get_id("i8").unwrap(),
                self.get_id("i16").unwrap(),
                self.get_id("i32").unwrap(),
                self.get_id("i64").unwrap(),
                self.get_id("f32").unwrap(),
                self.get_id("f64").unwrap(),
                self.get_id("float").unwrap(),
            ]),
            Type::Int(IntType::I64),
        )
        .expect("not initialized")
    }

    pub fn alloc_float_lit(&'a self) -> &'a InferTy<'a> {
        self.alloc_atom(
            "float",
            Vec::from([self.get_id("f32").unwrap(), self.get_id("f64").unwrap()]),
            Type::Float(FloatType::F64),
        )
        .expect("not initialized")
    }

    pub fn register_type<S: ToString>(&mut self, ty_name: S) -> Result<AtomTyID> {
        let ty_name = ty_name.to_string();
        if self.type_pool.contains_key(&ty_name) {
            return Err(TypeError::AlreadyDefinedType(ty_name));
        }
        let id = self.type_pool.len();
        self.type_pool.insert(ty_name, id);
        Ok(id)
    }

    pub fn remove_type<S: ToString>(&mut self, ty_name: S) {
        self.type_pool.remove(&ty_name.to_string());
    }

    fn alloc(&'a self, kind: TyKind<'a>) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::new(self.inner.len(), kind))
    }

    pub fn alloc_atom(
        &'a self,
        ty_name: &str,
        family: Vec<AtomTyID>,
        solve_ty: Type,
    ) -> Result<&'a InferTy<'a>> {
        self.type_pool
            .get(ty_name)
            .ok_or_else(|| TypeError::UnknownType(ty_name.to_string()))
            .map(|id| {
                self.alloc(TyKind::Atom(AtomTy {
                    id: *id,
                    name: ty_name.to_string(),
                    family,
                    solve_ty,
                }))
            })
    }

    pub fn alloc_any(&'a self) -> &'a InferTy<'a> {
        self.alloc(TyKind::Any)
    }

    pub fn alloc_ptr(&'a self, inner: &'a InferTy<'a>) -> &'a InferTy<'a> {
        self.alloc(TyKind::Ptr(inner))
    }

    pub fn alloc_fun(
        &'a self,
        args: Vec<&'a InferTy<'a>>,
        ret: &'a InferTy<'a>,
    ) -> &'a InferTy<'a> {
        self.alloc(TyKind::Fun(FunTy { args, ret }))
    }
}
