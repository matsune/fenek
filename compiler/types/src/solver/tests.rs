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
