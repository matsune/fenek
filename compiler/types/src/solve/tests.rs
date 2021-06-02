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
        ($a:expr, $b:expr) => {
            solver.bind($a, $b).unwrap();
        };
    }

    macro_rules! test_type {
        ($ty:ty => $($v:expr),*) => {
            $(
                assert_eq!(
                    solver.solve_type($v).unwrap().to_string(),
                    stringify!($ty)
                );
            )*
        } ;
        ($ty:ident => $($v:expr),*) => {
            $(
                assert_eq!(
                    solver.solve_type($v).unwrap().to_string(),
                    stringify!($ty)
                );
            )*
        } ;
        ($ty:literal => $($v:expr),*) => {
            $(
                assert_eq!(
                    solver.solve_type($v).unwrap().to_string(),
                    $ty
                );
            )*
        } ;
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

    // a -> b -> c
    //  d: i8 -> c
    //           c -> e
    //
    // => a = b = c = d = e = i8
    {
        alloc!(var: a, b, c, e);
        alloc!(i8: d);
        bind!(a, b);
        bind!(b, c);
        bind!(d, c);
        bind!(c, e);
        test_type!(i8 => a, b, c, d, e);
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
        bind!(a, b);
        bind!(b, c);
        bind!(c, d);
        test_type!(f32 => a, b, c, d);
    }

    // a -> b -> c
    //
    // => a = b = c
    // => unresolved
    {
        alloc!(var: a, b, c);
        bind!(a, b);
        bind!(b, c);
        test_error!(a, b, c);
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
        bind!(a, e);
        test_type!("(i64, i8) -> i32" => a, e);
    }

    // a
    // b = &a
    // c: i8
    //
    // a -> c
    //
    // => a = c = i8
    //    b = i8*
    {
        alloc!(var: a);
        let b = arena.alloc_ref(a);
        alloc!(i8: c);
        bind!(a, c);

        test_type!(i8 => a, c);
        test_type!("i8*" => b);
    }

    // var a
    // var b: i8* = &a
    //
    // a: i8
    // b: i8*
    //
    {
        alloc!(var: a);
        let b = arena.alloc_ref(arena.alloc_i8());
        bind!(b, arena.alloc_ref(a));
        test_type!(i8 => a);
        test_type!("i8*" => b);
    }

    // var a
    // var b = &a
    // var c = &b
    // d: i8 = *b
    //
    // a: i8
    // b: i8*
    // c: i8**
    // d: i8
    {
        alloc!(var: a, b, c);
        alloc!(i8: d);
        bind!(b, arena.alloc_ref(a));
        bind!(c, arena.alloc_ref(b));
        bind!(d, arena.alloc_deref(b));
        test_type!(i8 => a, d);
        test_type!("i8*" => b);
        test_type!("i8**" => c);
    }

    // var a
    // var b = *a
    // b = i8
    //
    // a: i8*
    // b: i8
    {
        alloc!(var: a, b);
        bind!(b, arena.alloc_deref(a));
        bind!(b, arena.alloc_i8());
        test_type!(i8 => b);
        test_type!("i8*" => a);
    }

    // var a
    // var b = *a
    // a = i8
    // b: error
    {
        alloc!(var: a, b);
        bind!(b, arena.alloc_deref(a));
        bind!(a, arena.alloc_i8());
        test_error!(b);
    }
}
