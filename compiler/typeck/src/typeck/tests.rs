use super::*;

#[test]
fn test_typeck() {
    let mut typeck = TypeCk::new();
    let node_id: NodeId = 0;
    let expr = Lit::new(node_id, LitKind::Int(1)).into();
    typeck.typecheck(&expr).unwrap();
    assert_eq!(*typeck.get_type(node_id).unwrap(), Type::Int(IntTy::ISize));
}
