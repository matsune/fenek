use std::str::Chars;

const EOF: char = '\0';

#[derive(Debug)]
struct Scanner<'a> {
    source: &'a mut Chars<'a>,
    peek: char,
    utf8_offset: usize,
}

impl<'a> Scanner<'a> {
    fn new(source: &'a mut Chars<'a>) -> Self {
        let peek = source.next().unwrap_or(EOF);
        Scanner {
            source,
            peek,
            utf8_offset: 0,
        }
    }

    fn peek(&mut self) -> char {
        self.peek
    }

    fn bump(&mut self) -> char {
        let p = self.peek;
        self.utf8_offset += p.len_utf8();
        self.peek = self.source.next().unwrap_or(EOF);
        p
    }

    fn is_eof(&self) -> bool {
        self.peek == EOF
    }

    fn utf8_offset(&self) -> usize {
        self.utf8_offset
    }
}

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
