use error::TypeError;
use std::cell::Cell;
use std::string::ToString;
use typed_arena::Arena;

type Result<T> = std::result::Result<T, TypeError>;

pub type StructID = usize;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int(IntType),
    Float(FloatType),
    Bool,
    // String,
    Ptr(Box<Type>),
    Void,
    Fun(FunType),
    Struct(StructType),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(inner) => inner.fmt(f),
            Self::Float(inner) => inner.fmt(f),
            Self::Bool => write!(f, "bool"),
            Self::Ptr(inner) => write!(f, "*{}", inner),
            Self::Void => write!(f, "void"),
            Self::Fun(inner) => inner.fmt(f),
            Self::Struct(inner) => inner.fmt(f),
        }
    }
}

impl Type {
    pub fn is_void(&self) -> bool {
        matches!(self, Type::Void)
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }

    pub fn is_fun(&self) -> bool {
        matches!(self, Type::Fun(_))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self, Type::Ptr(_))
    }

    pub fn into_fun(self) -> FunType {
        match self {
            Self::Fun(f) => f,
            _ => panic!(),
        }
    }

    pub fn as_fun(&self) -> &FunType {
        match self {
            Self::Fun(f) => f,
            _ => panic!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct MutType {
    pub is_mut: bool,
    pub ty: Type,
}

impl std::fmt::Display for MutType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", if self.is_mut { "mut" } else { "" }, self.ty)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunType {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

impl std::fmt::Display for FunType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}) -> {}",
            self.args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", "),
            self.ret
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub id: StructID,
    pub name: String,
    pub members: Vec<StructMember>,
}

impl std::fmt::Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "struct {} {{{}}}",
            self.name,
            self.members
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructMember {
    is_mut: bool,
    name: String,
    ty: Type,
}

impl std::fmt::Display for StructMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}: {}",
            if self.is_mut { "mut" } else { "" },
            self.name,
            self.ty
        )
    }
}

macro_rules! impl_Serialize_for_ToString {
    ($($name:ident),*) => {
        $(
            impl serde::Serialize for $name {
                fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    serializer.serialize_str(&self.to_string())
                }
            }
        )*
    }
}

impl_Serialize_for_ToString!(Type, IntType, FloatType, FunType, StructType, StructMember);

#[derive(Default)]
pub struct InferTyArena<'a> {
    pub inner: Arena<InferTy<'a>>,
}

impl<'a> InferTyArena<'a> {
    fn alloc_any(&'a self) -> &'a InferTy<'a> {
        let id = self.inner.len();
        self.inner.alloc(InferTy::Any(BindableTy::new(id)))
    }

    fn alloc_int_lit(&'a self) -> &'a InferTy<'a> {
        let id = self.inner.len();
        self.inner.alloc(InferTy::IntLit(BindableTy::new(id)))
    }

    fn alloc_float_lit(&'a self) -> &'a InferTy<'a> {
        let id = self.inner.len();
        self.inner.alloc(InferTy::FloatLit(BindableTy::new(id)))
    }

    pub fn alloc_ptr(&'a self, elem: &'a InferTy<'a>) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Ptr(elem))
    }

    pub fn alloc_fun(
        &'a self,
        args: Vec<&'a InferTy<'a>>,
        ret: &'a InferTy<'a>,
    ) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Fun(FunTy { args, ret }))
    }

    fn alloc_i8(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Int(IntType::I8))
    }

    fn alloc_i16(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Int(IntType::I16))
    }

    fn alloc_i32(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Int(IntType::I32))
    }

    fn alloc_i64(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Int(IntType::I64))
    }

    fn alloc_f32(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Float(FloatType::F32))
    }

    fn alloc_f64(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Float(FloatType::F64))
    }

    fn alloc_bool(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Bool)
    }

    fn alloc_void(&'a self) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Void)
    }

    fn alloc_struct(&'a self, struct_id: StructID) -> &'a InferTy<'a> {
        self.inner.alloc(InferTy::Struct(struct_id))
    }
}

pub type BindableTyID = usize;

#[derive(Debug, PartialEq)]
pub enum InferTy<'a> {
    Any(BindableTy<'a>),
    IntLit(BindableTy<'a>),
    FloatLit(BindableTy<'a>),
    Ptr(&'a InferTy<'a>),
    Fun(FunTy<'a>),
    Int(IntType),
    Float(FloatType),
    Bool,
    Void,
    Struct(StructID),
}

impl<'a> std::fmt::Display for InferTy<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any(_) => write!(f, "any"),
            Self::IntLit(_) => write!(f, "int_lit"),
            Self::FloatLit(_) => write!(f, "float_lit"),
            Self::Ptr(elem) => write!(f, "*{}", elem),
            Self::Fun(inner) => inner.fmt(f),
            Self::Int(inner) => inner.fmt(f),
            Self::Float(inner) => inner.fmt(f),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Struct(id) => write!(f, "struct {}", id),
        }
    }
}

impl<'a> InferTy<'a> {
    fn try_as_bindable(&self) -> Option<&BindableTy<'a>> {
        match self {
            Self::Any(inner) | Self::IntLit(inner) | Self::FloatLit(inner) => Some(inner),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct BindableTy<'a> {
    pub id: BindableTyID,
    next: Cell<Option<&'a InferTy<'a>>>,
}

impl<'a> PartialEq for BindableTy<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'a> BindableTy<'a> {
    fn new(id: BindableTyID) -> Self {
        Self {
            id,
            next: Cell::new(None),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunTy<'a> {
    pub args: Vec<&'a InferTy<'a>>,
    pub ret: &'a InferTy<'a>,
}

impl<'a> std::fmt::Display for FunTy<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} -> {}",
            self.args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            self.ret
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
}

impl std::fmt::Display for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FloatType {
    F32,
    F64,
}

impl std::fmt::Display for FloatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
        }
    }
}

struct Solver<'a> {
    arena: &'a InferTyArena<'a>,
    structs: Vec<StructType>,
}

impl<'a> Solver<'a> {
    pub fn new(arena: &'a InferTyArena<'a>) -> Self {
        Self {
            arena,
            structs: Vec::new(),
        }
    }

    pub fn add_struct<S: ToString>(&mut self, name: S, members: Vec<StructMember>) -> StructID {
        let id = self.structs.len();
        self.structs.push(StructType {
            id,
            name: name.to_string(),
            members,
        });
        id
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
            InferTy::Ptr(elem) => self.arena.alloc_ptr(self.prune(elem)),
            InferTy::Fun(fun) => {
                let mut args = Vec::with_capacity(fun.args.len());
                for arg in fun.args.iter() {
                    args.push(self.prune(arg));
                }
                let ret = self.prune(fun.ret);
                self.arena.alloc_fun(args, ret)
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
                self.arena.alloc_ptr(elem)
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
                self.arena.alloc_fun(arg_tys, ret_ty)
            }

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
        match &ty {
            InferTy::Any(_) => Err(TypeError::UnresolvedType),
            InferTy::IntLit(_) => Ok(Type::Int(IntType::I64)),
            InferTy::FloatLit(_) => Ok(Type::Float(FloatType::F64)),
            InferTy::Int(inner) => Ok(Type::Int(*inner)),
            InferTy::Float(inner) => Ok(Type::Float(*inner)),
            InferTy::Bool => Ok(Type::Bool),
            InferTy::Void => Ok(Type::Void),
            InferTy::Struct(id) => Ok(Type::Struct(self.structs[*id].clone())),
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

mod tests {
    use super::*;

    #[test]
    fn test_solve() {
        let arena = InferTyArena::default();
        let mut solver = Solver::new(&arena);

        macro_rules! alloc_fun {
            ($name: ident: ($($arg:ident),*) -> $ret:ident) => {
                let $name = arena.alloc_fun([$($arg,)*].into(), $ret);
            }
        }

        macro_rules! alloc {
            ($($name:ident),*: $ty:ident) => {
                $(
                    paste::expr! {
                        let $name = arena.[<alloc_ $ty>]();
                    }
                )*
            }
        }

        macro_rules! bind {
            ($a:expr, $b:expr) => {
                solver.bind($a, $b).unwrap();
            };
        }

        macro_rules! test_type {
            ($ty:literal => $($v:expr),*) => {
                $(
                    assert_eq!(
                        solver.solve_type($v).unwrap().to_string(),
                        $ty
                    );
                )*
            };
        }

        macro_rules! test_error {
            ($($v:expr),*) => {
                $(
                    let res = solver.solve_type($v);
                    assert!(res.is_err());
                    println!("{}", res.err().unwrap());
                )*
            }
        }

        {
            alloc!(a: i8);
            alloc!(b: any);
            bind!(a, b);
            test_type!("i8" => a, b);
        }

        {
            alloc!(a: int_lit);
            alloc!(b: any);
            alloc!(c: i64);
            bind!(a, b);
            bind!(b, c);
            test_type!("i64" => a, b, c);
        }

        {
            alloc!(a: int_lit);
            alloc!(b: float_lit);
            alloc!(c: f32);
            bind!(a, b);
            bind!(b, c);
            test_type!("f32" => a, b, c);
        }

        // a: (b, c) -> d
        // e: (f, g) -> h
        //
        // b: int lit
        // c: i8
        // d: i32
        // f: i64
        // g: any
        // h: any
        //
        // bind a with e
        //
        // a = e = (i64, i8) -> i32
        {
            alloc!(b: int_lit);
            alloc!(c: i8);
            alloc!(d: i32);
            alloc!(f: i64);
            alloc!(g, h: any);
            alloc_fun!(a: (b, c) -> d);
            alloc_fun!(e: (f, g) -> h);
            bind!(a, e);
            test_type!("(i64, i8) -> i32" => a, e);
        }

        {
            alloc!(a, b: any);
            bind!(a, b);
            test_error!(a, b);
        }

        // a: any
        // b = &a
        // => b is a pointer of a
        // c: i8
        //
        // bind a with c
        //
        // a = c = i8
        // b = *i8
        {
            alloc!(a: any);
            let b = arena.alloc_ptr(a);
            alloc!(c: i8);
            bind!(a, c);
            test_type!("i8" => a, c);
            test_type!("*i8" => b);
        }

        // a: any
        // b: *i8 = &a
        //
        // bind b with &a
        //
        // a: i8
        // b: i8*
        //
        {
            alloc!(a: any);
            let ref_a = arena.alloc_ptr(a);
            let b = arena.alloc_ptr(arena.alloc_i8());
            bind!(b, ref_a);
            test_type!("i8" => a);
            test_type!("*i8" => b);
        }

        // a: any
        // b: any = &a
        // c: any = &b
        // d: i8 = *b
        //
        // bind b with &a
        // bind c with &b
        // bind d with *b
        //
        // a: i8
        // b: *i8
        // c: **i8
        // d: i8
        {
            alloc!(a, b, c: any);
            alloc!(d: i8);
            bind!(b, arena.alloc_ptr(a));
            bind!(c, arena.alloc_ptr(b));
            // ('bind `d` with derefed `b`' means 'bind `b` with pointer of `d`')
            bind!(b, arena.alloc_ptr(d));
            test_type!("i8" => a, d);
            test_type!("*i8" => b);
            test_type!("**i8" => c);
        }

        // a: any
        // b: any = *a
        //
        // bind a with i8
        // bind b with *a
        //
        // a: i8
        // b: error
        {
            alloc!(a, b: any);
            bind!(a, arena.alloc_i8());
            // cannot bind i8 with pointer of any because i8 is not pointer
            assert!(solver.bind(a, arena.alloc_ptr(b)).is_err());
        }

        // a: A
        // b: any = &a
        // a: A
        // b: *A
        {
            let A = solver.add_struct("A", Vec::new());
            let a = arena.alloc_struct(A);
            alloc!(b: any);
            bind!(b, arena.alloc_ptr(a));
            test_type!("struct A {}" => a);
            test_type!("*struct A {}" => b);
        }
    }
}
