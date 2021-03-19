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
}

impl<'a> Lexer<'a> {
    fn new(source: &'a mut Chars<'a>) -> Self {
        Lexer {
            scanner: Scanner::new(source),
        }
    }

    fn scan(&mut self) -> Token {
        let c = self.scanner.peek();
        match c {
            EOF => {
                let mut tok = Token::default();
                tok.kind = TokenKind::Eof;
                tok
            }
            c if is_whitespace(c) => self.scan_while(TokenKind::Spaces, "".into(), is_whitespace),
            c if is_newline(c) => self.scan_while(TokenKind::Newlines, "".into(), is_newline),
            '/' => {
                let mut literal = String::from(self.scanner.bump());
                match self.scanner.peek() {
                    '/' => self.scan_while(TokenKind::LineComment, "/".into(), |c| !is_newline(c)),
                    _ => Token::new(TokenKind::Slash, "/".into()),
                }
            }
            ';' => self.scan_single(TokenKind::Semi),
            ',' => self.scan_single(TokenKind::Comma),
            '(' => self.scan_single(TokenKind::OpenParen),
            ')' => self.scan_single(TokenKind::CloseParen),
            '{' => self.scan_single(TokenKind::OpenBrace),
            '}' => self.scan_single(TokenKind::CloseBrace),
            ':' => self.scan_single(TokenKind::Colon),
            '=' => self.scan_single(TokenKind::Eq),
            '!' => self.scan_single(TokenKind::Not),
            '<' => self.scan_single(TokenKind::Lt),
            '>' => self.scan_single(TokenKind::Gt),
            '-' => self.scan_single(TokenKind::Minus),
            '&' => self.scan_single(TokenKind::And),
            '|' => self.scan_single(TokenKind::Or),
            '+' => self.scan_single(TokenKind::Plus),
            '*' => self.scan_single(TokenKind::Star),
            '^' => self.scan_single(TokenKind::Caret),
            '%' => self.scan_single(TokenKind::Percent),
            _ => Token::new(TokenKind::Unknown, String::from(c)),
        }
    }

    fn scan_single(&mut self, kind: TokenKind) -> Token {
        Token::new(kind, self.scanner.bump().into())
    }

    fn scan_while<F>(&mut self, kind: TokenKind, mut literal: String, predicate: F) -> Token
    where
        F: Fn(char) -> bool,
    {
        while self.scanner.peek() != EOF && predicate(self.scanner.peek()) {
            literal.push(self.scanner.bump());
        }
        Token::new(kind, literal)
    }
}

#[test]
fn test_lexer() {
    macro_rules! test_token {
        ($s:expr, $kind:ident) => {
            let tok = Lexer::new(&mut $s.chars()).scan();
            assert_eq!(tok, Token::new(TokenKind::$kind, $s.into()));
        };
    }

    test_token!(" \t", Spaces);
    test_token!("\n\n\n", Newlines);
    test_token!(r"// line comment", LineComment);
    test_token!("/", Slash);
    test_token!(";", Semi);
    test_token!(",", Comma);
    test_token!("(", OpenParen);
    test_token!(")", CloseParen);
    test_token!("{", OpenBrace);
    test_token!("}", CloseBrace);
    test_token!(":", Colon);
    test_token!("=", Eq);
    test_token!("!", Not);
    test_token!("<", Lt);
    test_token!(">", Gt);
    test_token!("-", Minus);
    test_token!("&", And);
    test_token!("|", Or);
    test_token!("+", Plus);
    test_token!("*", Star);
    test_token!("^", Caret);
    test_token!("%", Percent);
}
