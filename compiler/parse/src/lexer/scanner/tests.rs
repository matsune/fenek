use super::*;

#[test]
fn test_scanner_eof() {
    let mut source = "".chars();
    let s = Scanner::new(&mut source);
    assert_eq!(s.is_eof(), true);
}

#[test]
fn test_scanner_utf8() {
    let mut source = "a©あ😀".chars();
    let mut s = Scanner::new(&mut source);
    assert_eq!(s.is_eof(), false);
    assert_eq!(s.bump(), 'a');
    assert_eq!(s.utf8_offset(), 1);
    assert_eq!(s.bump(), '©');
    assert_eq!(s.utf8_offset(), 3);
    assert_eq!(s.bump(), 'あ');
    assert_eq!(s.utf8_offset(), 6);
    assert_eq!(s.bump(), '😀');
    assert_eq!(s.utf8_offset(), 10);
    assert_eq!(s.is_eof(), true);
}
