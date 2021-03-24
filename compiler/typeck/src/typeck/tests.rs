use super::*;

#[test]
fn test_typeck() {
    let mut typeck = TypeCk::new();
    let node_id: NodeId = 0;
    let expr = Lit::new(node_id, LitKind::Int(1)).into();
    typeck.typecheck_expr(&expr).unwrap();
    assert_eq!(*typeck.get_type(node_id).unwrap(), Type::Int(IntTy::ISize));

    let mut typeck = TypeCk::new();
    let node_id: NodeId = 0;
    let expr = Lit::new(node_id, LitKind::Float(1.3)).into();
    typeck.typecheck_expr(&expr).unwrap();
    assert_eq!(
        *typeck.get_type(node_id).unwrap(),
        Type::Float(FloatTy::F64)
    );

    let mut typeck = TypeCk::new();
    let node_id: NodeId = 0;
    let expr = Lit::new(node_id, LitKind::Bool(true)).into();
    typeck.typecheck_expr(&expr).unwrap();
    assert_eq!(*typeck.get_type(node_id).unwrap(), Type::Bool);

    let mut typeck = TypeCk::new();
    let node_id: NodeId = 0;
    let expr = Lit::new(node_id, LitKind::String("".to_string())).into();
    typeck.typecheck_expr(&expr).unwrap();
    assert_eq!(*typeck.get_type(node_id).unwrap(), Type::String);

    let mut typeck = TypeCk::new();
    let expr = Lit::new(2, LitKind::Int(1)).into();
    let stmt = VarDecl::new(0, Ident::new(1, "a".to_string()), expr).into();
    assert_eq!(typeck.typecheck_stmt(&stmt).unwrap(), Type::Void);
    assert_eq!(*typeck.get_type(2).unwrap(), Type::Int(IntTy::ISize));
    assert_eq!(
        typeck
            .current_scope()
            .lookup("a")
            .unwrap()
            .into_var_def()
            .ty,
        Type::Int(IntTy::ISize)
    );
}
