use super::*;

#[test]
fn test_scanner_utf8() {
    let mut s = Scanner::new("a©あ😀".chars());
    assert_eq!(s.bump(), 'a');
    assert_eq!(s.bump(), '©');
    assert_eq!(s.bump(), 'あ');
    assert_eq!(s.bump(), '😀');
}
