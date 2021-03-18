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
            c if is_whitespace(c) => self.scan_while(TokenKind::Space, is_whitespace),
            c if is_newline(c) => self.scan_while(TokenKind::Newline, is_newline),
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
            _ => Token::new(TokenKind::Unknown, String::from(c), self.pos, self.pos),
        }
    }

    fn scan_single(&mut self, kind: TokenKind) -> Token {
        let pos = self.pos;
        self.pos.add_row(1);
        Token::new(kind, String::from(self.scanner.bump()), pos, pos)
    }

    fn scan_while<F>(&mut self, kind: TokenKind, predicate: F) -> Token
    where
        F: Fn(char) -> bool,
    {
        let mut literal = String::default();
        let begin = self.pos;
        let mut end = self.pos;
        while predicate(self.scanner.peek()) {
            end = self.pos;
            let c = self.scanner.bump();
            if is_newline(c) {
                self.pos.newline();
            } else {
                self.pos.add_row(1);
            }
            literal.push(c);
        }
        Token::new(kind, literal, begin, end)
    }
}

#[test]
fn test_lexer() {
    let mut source = r"   

;,(){}:=!<>-&|+*^%"
        .chars();
    let mut l = Lexer::new(&mut source);

    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Space, "   ".into(), Pos(1, 1), Pos(1, 3))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(
            TokenKind::Newline,
            r"

"
            .into(),
            Pos(1, 4),
            Pos(2, 1)
        )
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Semi, ";".into(), Pos(3, 1), Pos(3, 1))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Comma, ",".into(), Pos(3, 2), Pos(3, 2))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::OpenParen, "(".into(), Pos(3, 3), Pos(3, 3))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::CloseParen, ")".into(), Pos(3, 4), Pos(3, 4))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::OpenBrace, "{".into(), Pos(3, 5), Pos(3, 5))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::CloseBrace, "}".into(), Pos(3, 6), Pos(3, 6))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Colon, ":".into(), Pos(3, 7), Pos(3, 7))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Eq, "=".into(), Pos(3, 8), Pos(3, 8))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Not, "!".into(), Pos(3, 9), Pos(3, 9))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Lt, "<".into(), Pos(3, 10), Pos(3, 10))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Gt, ">".into(), Pos(3, 11), Pos(3, 11))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Minus, "-".into(), Pos(3, 12), Pos(3, 12))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::And, "&".into(), Pos(3, 13), Pos(3, 13))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Or, "|".into(), Pos(3, 14), Pos(3, 14))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Plus, "+".into(), Pos(3, 15), Pos(3, 15))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Star, "*".into(), Pos(3, 16), Pos(3, 16))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Caret, "^".into(), Pos(3, 17), Pos(3, 17))
    );
    let tok = l.scan();
    assert_eq!(
        tok,
        Token::new(TokenKind::Percent, "%".into(), Pos(3, 18), Pos(3, 18))
    );
}
