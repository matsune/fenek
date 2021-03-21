use super::*;
use crate::lexer::Lexer;

#[test]
fn test_parse_expr() {
    let mut chars = "1 + a * 2".chars();
    let tokens = Lexer::new(&mut chars).lex().unwrap();
    let mut parser = Parser::new(tokens.into());
    let binary = parser.parse_expr().unwrap().unwrap_binary();
    assert_eq!(binary.op, BinOpKind::Add);
    assert_eq!(binary.lhs.unwrap_lit().raw, "1");
    let rhs = binary.rhs.unwrap_binary();
    assert_eq!(rhs.op, BinOpKind::Mul);
    assert_eq!(rhs.lhs.unwrap_ident().raw, "a");
    assert_eq!(rhs.rhs.unwrap_lit().raw, "2");

    let mut chars = "1 * a - 2".chars();
    let tokens = Lexer::new(&mut chars).lex().unwrap();
    let mut parser = Parser::new(tokens.into());
    let binary = parser.parse_expr().unwrap().unwrap_binary();
    assert_eq!(binary.op, BinOpKind::Sub);
    let lhs = binary.lhs.unwrap_binary();
    assert_eq!(lhs.op, BinOpKind::Mul);
    assert_eq!(lhs.lhs.unwrap_lit().raw, "1");
    assert_eq!(lhs.rhs.unwrap_ident().raw, "a");
    assert_eq!(binary.rhs.unwrap_lit().raw, "2");

    let mut chars = "b - 3 * a / 2".chars();
    let tokens = Lexer::new(&mut chars).lex().unwrap();
    let mut parser = Parser::new(tokens.into());
    let binary = parser.parse_expr().unwrap().unwrap_binary();
    assert_eq!(binary.op, BinOpKind::Sub);
    assert_eq!(binary.lhs.unwrap_ident().raw, "b");
    let rhs_binary = binary.rhs.unwrap_binary();
    assert_eq!(rhs_binary.op, BinOpKind::Mul);
    assert_eq!(rhs_binary.lhs.unwrap_lit().raw, "3");
    let rhs_lhs_binary = rhs_binary.rhs.unwrap_binary();
    assert_eq!(rhs_lhs_binary.op, BinOpKind::Div);
    assert_eq!(rhs_lhs_binary.lhs.unwrap_ident().raw, "a");
    assert_eq!(rhs_lhs_binary.rhs.unwrap_lit().raw, "2");

    let mut chars = "((4 - 3) * 1) + 2".chars();
    let tokens = Lexer::new(&mut chars).lex().unwrap();
    let mut parser = Parser::new(tokens.into());
    let binary = parser.parse_expr().unwrap().unwrap_binary();
    assert_eq!(binary.op, BinOpKind::Add);
    assert_eq!(binary.rhs.unwrap_lit().raw, "2");
    let lhs_binary = binary.lhs.unwrap_binary();
    assert_eq!(lhs_binary.op, BinOpKind::Mul);
    assert_eq!(lhs_binary.rhs.unwrap_lit().raw, "1");
    let lhs_lhs_binary = lhs_binary.lhs.unwrap_binary();
    assert_eq!(lhs_lhs_binary.op, BinOpKind::Sub);
    assert_eq!(lhs_lhs_binary.lhs.unwrap_lit().raw, "4");
    assert_eq!(lhs_lhs_binary.rhs.unwrap_lit().raw, "3");
}
