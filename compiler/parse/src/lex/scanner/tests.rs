use super::*;

#[test]
fn test_scanner_utf8() {
    let mut source = "a©あ😀".chars();
    let mut s = Scanner::new(&mut source);
    assert_eq!(s.bump(), 'a');
    assert_eq!(s.bump(), '©');
    assert_eq!(s.bump(), 'あ');
    assert_eq!(s.bump(), '😀');
}
