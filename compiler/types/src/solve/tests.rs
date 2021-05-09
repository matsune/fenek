use super::*;

#[test]
fn test_solve() {
    let arena = InferTyArena::default();
    let solver = Solver::new(&arena);

    macro_rules! alloc {
        ($ty:ident: $($name:ident),*) => {
            $(
                paste::expr! {
                    let $name = arena.[<alloc_ $ty>]();
                }
            )*
        }
    }
    macro_rules! bind {
        ($a:ident -> $b:expr) => {
            solver.bind($a, $b).unwrap();
        };
    }

    macro_rules! test_type {
        ($($v:expr),* ; $ty:expr) => {
            $(
                assert_eq!(
                    solver.solve_type($v).unwrap(),
                    $ty
                );
            )*
        }
    }

    macro_rules! test_unresolved {
        ($($v:expr),*) => {
            $(
                assert!(solver.solve_type($v).is_err());
            )*
        }
    }

    // a -> b -> c
    //  d: i8 -> c
    //           c -> e
    //
    // => a = b = c = d = e = i8
    {
        alloc!(var: a, b, c, e);
        alloc!(i8: d);
        bind!(a -> b);
        bind!(b -> c);
        bind!(d -> c);
        bind!(c -> e);
        test_type!(a, b, c, d, e; Type::Int(IntType::I8));
    }

    // a -> b
    // b -> c
    // c -> d
    //
    // b = int_lit
    // c = float_lit
    // d = f32
    //
    // => a = b = c = d = f32
    {
        alloc!(var: a);
        alloc!(int_lit: b);
        alloc!(float_lit: c);
        alloc!(f32: d);
        bind!(a -> b);
        bind!(b -> c);
        bind!(c -> d);
        test_type!(a, b, c, d; Type::Float(FloatType::F32));
    }

    // a -> b -> c
    //
    // => a = b = c
    // => unresolved
    {
        alloc!(var: a, b, c);
        bind!(a -> b);
        bind!(b -> c);
        test_unresolved!(a, b, c);
    }

    // a: (b: int, c: i8) -> d: i32
    // e: (f: i64, g) -> h: int
    //
    // a -> e
    //
    // => a = e = (i64, i8) -> i32
    {
        alloc!(int_lit: b, h);
        alloc!(i8: c);
        alloc!(i32: d);
        alloc!(i64: f);
        alloc!(var: g);
        let a = arena.alloc_fun([b, c].into(), d);
        let e = arena.alloc_fun([f, g].into(), h);
        bind!(a -> e);
        test_type!(a, e; Type::Fun(FunType::new(
            [Type::Int(IntType::I64), Type::Int(IntType::I8)].into(),
            Box::new(Type::Int(IntType::I32)),
        )));
    }

    // a
    // b = &a
    // c: i8
    //
    // a -> c
    //
    // => a = c = i8
    //    b = &i8
    {
        alloc!(var: a);
        let b = arena.alloc_ref(a);
        alloc!(i8: c);
        bind!(a -> c);

        test_type!(a, c; Type::Int(IntType::I8));
        test_type!(b; Type::Ref(Box::new(Type::Int(IntType::I8))));
    }

    // var a
    // var b = &a
    // var c = &b
    // c = x: i8
    //
    // a: int
    // b: &int
    // c: &int
    //
    {
        alloc!(var: a, b, c);
        bind!(b -> arena.alloc_ref(a.prune().deref()));
        bind!(c -> arena.alloc_ref(b.prune().deref()));
        let x = arena.alloc_i8();
        bind!(x -> c.prune().deref());
        test_type!(a; Type::Int(IntType::I8));
        test_type!(b, c; Type::Ref(Box::new(Type::Int(IntType::I8))));
    }
}
