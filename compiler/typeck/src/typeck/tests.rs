use super::*;
use error::Pos;
use mir::{FloatTy, IntTy, Type};
use parse::ast::*;

#[test]
fn test_typeck() {
    let mut typeck = TypeCk::new();
    let node_id: NodeId = 0;
    let expr = Lit::new(node_id, LitKind::Int(1), Pos::default()).into();
    typeck.typecheck_expr(&expr).unwrap();
    assert_eq!(*typeck.get_type(node_id).unwrap(), Type::Int(IntTy::I64));

    let mut typeck = TypeCk::new();
    let node_id: NodeId = 0;
    let expr = Lit::new(node_id, LitKind::Float(1.3), Pos::default()).into();
    typeck.typecheck_expr(&expr).unwrap();
    assert_eq!(
        *typeck.get_type(node_id).unwrap(),
        Type::Float(FloatTy::F64)
    );

    let mut typeck = TypeCk::new();
    let node_id: NodeId = 0;
    let expr = Lit::new(node_id, LitKind::Bool(true), Pos::default()).into();
    typeck.typecheck_expr(&expr).unwrap();
    assert_eq!(*typeck.get_type(node_id).unwrap(), Type::Bool);

    let mut typeck = TypeCk::new();
    let node_id: NodeId = 0;
    let expr = Lit::new(node_id, LitKind::String("".to_string()), Pos::default()).into();
    typeck.typecheck_expr(&expr).unwrap();
    assert_eq!(*typeck.get_type(node_id).unwrap(), Type::String);

    let mut typeck = TypeCk::new();
    let expr = Lit::new(2, LitKind::Int(1), Pos::default()).into();
    let stmt = VarDecl::new(0, Ident::new(1, "a".to_string(), Pos::default()), expr).into();
    typeck.typecheck_stmt(&stmt).unwrap();
    assert_eq!(*typeck.get_type(2).unwrap(), Type::Int(IntTy::I64));
    assert_eq!(
        typeck
            .current_scope()
            .lookup("a")
            .unwrap()
            .into_var_def()
            .ty,
        Type::Int(IntTy::I64)
    );

    let mut typeck = TypeCk::new();
    let lhs = Lit::new(1, LitKind::Int(1), Pos::default()).into();
    let rhs = Lit::new(2, LitKind::Int(2), Pos::default()).into();
    let expr = Expr::Binary(Binary::new(0, BinOp::new("+".to_string(), 10), lhs, rhs));
    assert_eq!(
        typeck.typecheck_expr(&expr).unwrap().get_type(),
        Type::Int(IntTy::I64)
    );
}

#[test]
fn test_typeck_error() {
    let mut typeck = TypeCk::new();
    let lhs = Lit::new(1, LitKind::Int(1), Pos::default()).into();
    let rhs = Lit::new(2, LitKind::Bool(true), Pos::default()).into();
    let expr = Expr::Binary(Binary::new(0, BinOp::new("+".to_string(), 10), lhs, rhs));
    assert_eq!(
        typeck.typecheck_expr(&expr).unwrap_err().to_string(),
        "1:1: invalid binary types"
    );
}
