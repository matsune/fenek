use super::*;
use crate::lexer::Lexer;

#[test]
fn test_parse_expr() {
    let mut chars = "1".chars();
    let tokens = Lexer::new(&mut chars).lex().unwrap();
    let mut parser = Parser::new(tokens.into());
    let expr = parser.parse_expr().unwrap();
    match expr.kind {
        ExprKind::Lit(lit) => assert_eq!(lit, Lit::new(LitKind::Int, "1".into())),
        _ => panic!(),
    };
}
