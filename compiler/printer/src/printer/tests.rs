use super::*;

#[test]
fn test_printer() {
    let node = parse::parse("var a = 3 * 4 / 10 + 2;").unwrap();
    print(&node);
}
