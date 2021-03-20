use super::*;

#[test]
fn test_parse_expr() {
    let mut chars = "1".chars();
    let mut parser = Parser::new(&mut chars);
    let expr = parser.parse_expr().unwrap();
    match expr.kind {
        ExprKind::Lit(lit) => assert_eq!(lit, Lit::new(LitKind::Int, "1".into())),
        _ => panic!(),
    };
}
