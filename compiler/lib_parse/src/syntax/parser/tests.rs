use super::*;
use crate::lexer::Lexer;

#[test]
fn test_parse_expr() {
    let mut chars = "1 + a".chars();
    let tokens = Lexer::new(&mut chars).lex().unwrap();
    let mut parser = Parser::new(tokens.into());
    let binary = parser.parse_expr().unwrap();
    match binary.kind {
        ExprKind::Binary(BinOpKind::Add, left, right) => match left.kind {
            ExprKind::Lit(lit) => assert_eq!(lit, Lit::new(LitKind::Int, "1".into())),
            _ => panic!("not lit"),
        },
        _ => panic!("not binary"),
    };
}
