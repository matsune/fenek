use super::*;
use crate::lexer::Lexer;

macro_rules! make_parser {
    ($s:expr) => {
        Parser::new(Lexer::new(&mut $s.chars()).lex().unwrap().into())
    };
}

#[test]
fn test_parse_expr() {
    let mut parser = make_parser!("a");
    let ident = parser.parse_expr().unwrap().unwrap_ident();
    assert_eq!(ident.raw, "a");
}

#[test]
fn test_parse_expr1() {
    let mut parser = make_parser!("1 + a * 2");
    let binary = parser.parse_expr().unwrap().unwrap_binary();
    assert_eq!(binary.op, BinOp::Add);
    assert_eq!(binary.lhs.unwrap_lit().raw, "1");
    let rhs = binary.rhs.unwrap_binary();
    assert_eq!(rhs.op, BinOp::Mul);
    assert_eq!(rhs.lhs.unwrap_ident().raw, "a");
    assert_eq!(rhs.rhs.unwrap_lit().raw, "2");
}

#[test]
fn test_parse_expr2() {
    let mut parser = make_parser!("1 * a - 2");
    let binary = parser.parse_expr().unwrap().unwrap_binary();
    assert_eq!(binary.op, BinOp::Sub);
    let lhs = binary.lhs.unwrap_binary();
    assert_eq!(lhs.op, BinOp::Mul);
    assert_eq!(lhs.lhs.unwrap_lit().raw, "1");
    assert_eq!(lhs.rhs.unwrap_ident().raw, "a");
    assert_eq!(binary.rhs.unwrap_lit().raw, "2");
}

#[test]
fn test_parse_expr3() {
    let mut parser = make_parser!("b - 3 * a / 2");
    let binary = parser.parse_expr().unwrap().unwrap_binary();
    assert_eq!(binary.op, BinOp::Sub);
    assert_eq!(binary.lhs.unwrap_ident().raw, "b");
    let rhs_binary = binary.rhs.unwrap_binary();
    assert_eq!(rhs_binary.op, BinOp::Mul);
    assert_eq!(rhs_binary.lhs.unwrap_lit().raw, "3");
    let rhs_lhs_binary = rhs_binary.rhs.unwrap_binary();
    assert_eq!(rhs_lhs_binary.op, BinOp::Div);
    assert_eq!(rhs_lhs_binary.lhs.unwrap_ident().raw, "a");
    assert_eq!(rhs_lhs_binary.rhs.unwrap_lit().raw, "2");
}

#[test]
fn test_parse_expr4() {
    let mut parser = make_parser!("((4 - 3) * 1) + 2");
    let binary = parser.parse_expr().unwrap().unwrap_binary();
    assert_eq!(binary.op, BinOp::Add);
    assert_eq!(binary.rhs.unwrap_lit().raw, "2");
    let lhs_binary = binary.lhs.unwrap_binary();
    assert_eq!(lhs_binary.op, BinOp::Mul);
    assert_eq!(lhs_binary.rhs.unwrap_lit().raw, "1");
    let lhs_lhs_binary = lhs_binary.lhs.unwrap_binary();
    assert_eq!(lhs_lhs_binary.op, BinOp::Sub);
    assert_eq!(lhs_lhs_binary.lhs.unwrap_lit().raw, "4");
    assert_eq!(lhs_lhs_binary.rhs.unwrap_lit().raw, "3");
}

#[test]
fn test_parse_expr5() {
    let mut parser = make_parser!("!(4 - 3)");
    let unary = parser.parse_expr().unwrap().unwrap_unary();
    assert_eq!(unary.op, UnaryOp::Not);
    let binary = unary.expr.unwrap_binary();
    assert_eq!(binary.op, BinOp::Sub);
    assert_eq!(binary.lhs.unwrap_lit().raw, "4");
    assert_eq!(binary.rhs.unwrap_lit().raw, "3");
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
    assert_eq!(var_decl.init.unwrap_lit().raw, "3");
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
