use super::scanner::{Scanner, EOF};
use super::token::{Pos, Token, TokenKind};
use std::str::Chars;

/// '\n' | '\r'
fn is_newline(c: char) -> bool {
    c == '\n' || c == '\r'
}

/// ' ' | '\t'
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t'
}

/// '0' ~ '9'
fn is_dec_digit(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

struct Lexer<'a> {
    scanner: Scanner<'a>,
    pos: Pos,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a mut Chars<'a>) -> Self {
        Lexer {
            scanner: Scanner::new(source),
            pos: Pos::default(),
        }
    }

    fn scan(&mut self) -> Token {
        let c = self.scanner.peek();
        match c {
            EOF => {
                let mut tok = Token::default();
                tok.kind = TokenKind::Eof;
                tok.begin = self.pos;
                tok.end = self.pos;
                tok
            }
            c if is_whitespace(c) => self.scan_whitespace(),
            c if is_newline(c) => self.scan_newline(),
            _ => {
                let mut tok = Token::default();
                tok
            }
        }
    }

    fn scan_whitespace(&mut self) -> Token {
        let kind = TokenKind::Whitespace;
        let mut literal = String::default();
        let begin = self.pos;
        let mut end = self.pos;
        while is_whitespace(self.scanner.peek()) {
            end = self.pos;
            self.pos.add_row(1);
            literal.push(self.scanner.bump());
        }
        Token::new(kind, literal, begin, end)
    }

    fn scan_newline(&mut self) -> Token {
        let kind = TokenKind::Newline;
        let mut literal = String::default();
        let begin = self.pos;
        let mut end = self.pos;
        while is_newline(self.scanner.peek()) {
            end = self.pos;
            self.pos.newline();
            literal.push(self.scanner.bump());
        }
        Token::new(kind, literal, begin, end)
    }
}

#[test]
fn test_lexer() {
    let mut source = r"   

"
    .chars();
    let mut l = Lexer::new(&mut source);
    let tok = l.scan();
    assert_eq!(tok.kind, TokenKind::Whitespace);
    assert_eq!(tok.literal, "   ");
    assert_eq!(tok.begin, Pos(1, 1));
    assert_eq!(tok.end, Pos(1, 3));
    let tok = l.scan();
    assert_eq!(tok.kind, TokenKind::Newline);
    assert_eq!(
        tok.literal,
        r"

"
    );
    assert_eq!(tok.begin, Pos(1, 4));
    assert_eq!(tok.end, Pos(2, 1));
    let tok = l.scan();
    assert_eq!(tok.kind, TokenKind::Eof);
    assert_eq!(tok.literal, "");
    assert_eq!(tok.begin, Pos(3, 1));
    assert_eq!(tok.end, Pos(3, 1));
}
