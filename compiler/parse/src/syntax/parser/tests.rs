use super::*;
use crate::lexer::Lexer;

macro_rules! make_parser {
    ($s:expr) => {
        Parser::new(Lexer::new(&mut $s.chars()).lex().unwrap().into())
    };
}

#[test]
fn test_parse_expr_lit_int() {
    macro_rules! test {
        ($s:expr, $num:expr) => {
            let mut parser = make_parser!($s);
            assert_eq!($num, parser.parse_expr().unwrap().into_lit().into_int());
        };
    }

    test!("12", 12);
    test!("123_456_789", 123456789);
    test!("100___", 100);
    test!("9223372036854775807", 9223372036854775807);

    test!("0b0", 0);
    test!("0b_1_1", 0b11);
    test!("0b0011_0011_", 0b00110011);

    test!("0o71", 0o71);
    test!("0o123_456", 0o123456);
    test!("0o100___", 0o100);

    test!("0x0", 0);
    test!("0x4F", 0x4F);
    test!("0x2d_aa", 0x2daa);
}

#[test]
fn test_parse_expr_lit_int_error() {
    macro_rules! test {
        ($s:expr, $err:expr) => {
            let mut parser = make_parser!($s);
            assert_eq!($err, parser.parse_expr().unwrap_err().to_string());
        };
    }
    test!(
        "9223372036854775808",
        "constant 9223372036854775808 overflows int"
    );
    test!(
        "0b11111111111111111111111111111111111111111111111111111111111111111111111111",
        "constant 0b11111111111111111111111111111111111111111111111111111111111111111111111111 overflows int"
    );
}

#[test]
fn test_parse_expr_lit_float() {
    macro_rules! test {
        ($s:expr, $num:expr) => {
            let mut parser = make_parser!($s);
            assert_eq!($num, parser.parse_expr().unwrap().into_lit().into_float());
        };
    }

    test!("1.2", 1.2);
    test!("1_.2_3_", 1.23);
}

#[test]
fn test_parse_expr1() {
    let mut parser = make_parser!("1 + a * 2");
    let binary = parser.parse_expr().unwrap().into_binary();
    assert_eq!(binary.op, BinOp::Add);
    assert_eq!(binary.lhs.into_lit().into_int(), 1);
    let rhs = binary.rhs.into_binary();
    assert_eq!(rhs.op, BinOp::Mul);
    assert_eq!(rhs.lhs.into_ident().raw, "a");
    assert_eq!(rhs.rhs.into_lit().into_int(), 2);

    let mut parser = make_parser!("1 * a - 2");
    let binary = parser.parse_expr().unwrap().into_binary();
    assert_eq!(binary.op, BinOp::Sub);
    let lhs = binary.lhs.into_binary();
    assert_eq!(lhs.op, BinOp::Mul);
    assert_eq!(lhs.lhs.into_lit().into_int(), 1);
    assert_eq!(lhs.rhs.into_ident().raw, "a");
    assert_eq!(binary.rhs.into_lit().into_int(), 2);

    let mut parser = make_parser!("b - 3 * a / 2");
    let binary = parser.parse_expr().unwrap().into_binary();
    assert_eq!(binary.op, BinOp::Sub);
    assert_eq!(binary.lhs.into_ident().raw, "b");
    let rhs_binary = binary.rhs.into_binary();
    assert_eq!(rhs_binary.op, BinOp::Mul);
    assert_eq!(rhs_binary.lhs.into_lit().into_int(), 3);
    let rhs_lhs_binary = rhs_binary.rhs.into_binary();
    assert_eq!(rhs_lhs_binary.op, BinOp::Div);
    assert_eq!(rhs_lhs_binary.lhs.into_ident().raw, "a");
    assert_eq!(rhs_lhs_binary.rhs.into_lit().into_int(), 2);

    let mut parser = make_parser!("((4 - 3) * 1) + 2");
    let binary = parser.parse_expr().unwrap().into_binary();
    assert_eq!(binary.op, BinOp::Add);
    assert_eq!(binary.rhs.into_lit().into_int(), 2);
    let lhs_binary = binary.lhs.into_binary();
    assert_eq!(lhs_binary.op, BinOp::Mul);
    assert_eq!(lhs_binary.rhs.into_lit().into_int(), 1);
    let lhs_lhs_binary = lhs_binary.lhs.into_binary();
    assert_eq!(lhs_lhs_binary.op, BinOp::Sub);
    assert_eq!(lhs_lhs_binary.lhs.into_lit().into_int(), 4);
    assert_eq!(lhs_lhs_binary.rhs.into_lit().into_int(), 3);

    let mut parser = make_parser!("!(4 - 3)");
    let unary = parser.parse_expr().unwrap().into_unary();
    assert_eq!(unary.op, UnaryOp::Not);
    let binary = unary.expr.into_binary();
    assert_eq!(binary.op, BinOp::Sub);
    assert_eq!(binary.lhs.into_lit().into_int(), 4);
    assert_eq!(binary.rhs.into_lit().into_int(), 3);
}

#[test]
fn test_parse_expr_error() {
    macro_rules! parse_error {
        ($s:expr,$e:expr) => {
            assert_eq!(
                Parser::new(Lexer::new(&mut $s.chars()).lex().unwrap().into())
                    .parse_expr()
                    .unwrap_err()
                    .to_string(),
                $e
            );
        };
    }
    parse_error!("(4 - 3", "unclosed expr");
    parse_error!("4 - ", "invalid expr");
}

#[test]
fn test_parse_var_decl() {
    let mut parser = make_parser!("var a = 3");
    let var_decl = parser.parse_var_decl().unwrap();
    assert_eq!(var_decl.name.raw, "a");
    assert_eq!(var_decl.init.into_lit().into_int(), 3);
}

#[test]
fn test_parse_var_decl_error() {
    let mut parser = make_parser!("var= 3");
    assert_eq!(
        parser.parse_var_decl().unwrap_err().to_string(),
        "expected spaces"
    );

    let mut parser = make_parser!("var = 3");
    assert_eq!(
        parser.parse_var_decl().unwrap_err().to_string(),
        "expected ident"
    );

    let mut parser = make_parser!("var a");
    assert_eq!(
        parser.parse_var_decl().unwrap_err().to_string(),
        "expected `=`"
    );
}
