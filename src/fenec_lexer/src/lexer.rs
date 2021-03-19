use super::scanner::{Scanner, EOF};
use super::token::{Literal, Pos, Token, TokenKind};
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
        let c = self.scanner.bump();
        match c {
            EOF => Token::new(TokenKind::Eof, EOF.into()),
            c if is_whitespace(c) => self.scan_while(TokenKind::Spaces, c.into(), is_whitespace),
            c if is_newline(c) => self.scan_while(TokenKind::Newlines, c.into(), is_newline),
            '/' => match self.scanner.peek() {
                '/' => self.scan_while(TokenKind::LineComment, c.into(), |c| !is_newline(c)),
                _ => Token::new(TokenKind::Slash, c.into()),
            },
            ';' => Token::new(TokenKind::Semi, c.into()),
            ',' => Token::new(TokenKind::Comma, c.into()),
            '(' => Token::new(TokenKind::OpenParen, c.into()),
            ')' => Token::new(TokenKind::CloseParen, c.into()),
            '{' => Token::new(TokenKind::OpenBrace, c.into()),
            '}' => Token::new(TokenKind::CloseBrace, c.into()),
            ':' => Token::new(TokenKind::Colon, c.into()),
            '=' => Token::new(TokenKind::Eq, c.into()),
            '!' => Token::new(TokenKind::Not, c.into()),
            '<' => Token::new(TokenKind::Lt, c.into()),
            '>' => Token::new(TokenKind::Gt, c.into()),
            '-' => Token::new(TokenKind::Minus, c.into()),
            '&' => Token::new(TokenKind::And, c.into()),
            '|' => Token::new(TokenKind::Or, c.into()),
            '+' => Token::new(TokenKind::Plus, c.into()),
            '*' => Token::new(TokenKind::Star, c.into()),
            '^' => Token::new(TokenKind::Caret, c.into()),
            '%' => Token::new(TokenKind::Percent, c.into()),
            c if is_dec_digit(c) => {
                self.scan_while(TokenKind::Literal(Literal::Number), c.into(), is_dec_digit)
            }
            '"' => self.scan_string(),
            '\'' => self.scan_char(),
            _ => Token::new(TokenKind::Unknown, c.into()),
        }
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

    fn scan_string(&mut self) -> Token {
        let mut literal = "\"".to_string();
        loop {
            let c = self.scanner.bump();
            match c {
                '"' => {
                    literal.push(c);
                    break;
                }
                // '\' [ '\"' | common_escape ]
                '\\' => {
                    // common_escape : '\'
                    //               | 'n' | 'r' | 't' | '0'
                    //               | 'x' hex_digit 2
                    let escaped_c = match self.scanner.bump() {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '0' => '\0',
                        'x' => unimplemented!("hex_digit"),
                        c => unreachable!("error: {}", c),
                    };
                    literal.push(escaped_c);
                }
                EOF => break,
                _ => literal.push(c),
            }
        }
        Token::new(TokenKind::Literal(Literal::String), literal)
    }

    fn scan_char(&mut self) -> Token {
        let mut literal = "'".to_string();
        loop {
            let c = self.scanner.bump();
            match c {
                '\'' => {
                    literal.push(c);
                    break;
                }
                // '\' [ '\'' | common_escape ]
                '\\' => {
                    // common_escape : '\'
                    //               | 'n' | 'r' | 't' | '0'
                    //               | 'x' hex_digit 2
                    let escaped_c = match self.scanner.bump() {
                        '\'' => '\'',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '0' => '\0',
                        'x' => unimplemented!("hex_digit"),
                        c => unreachable!("error: {}", c),
                    };
                    literal.push(escaped_c);
                }
                EOF => break,
                _ => literal.push(c),
            }
        }
        Token::new(TokenKind::Literal(Literal::Char), literal)
    }
}

#[test]
fn test_lexer() {
    macro_rules! test_token {
        ($s:expr, $kind:expr) => {
            let tok = Lexer::new(&mut $s.chars()).scan();
            assert_eq!(tok, Token::new($kind, $s.into()));
        };
    }
    macro_rules! test_token_literal {
        ($s:expr, $kind:expr, $lit:expr) => {
            let tok = Lexer::new(&mut $s.chars()).scan();
            assert_eq!(tok, Token::new($kind, $lit.into()));
        };
    }

    test_token!(" \t", TokenKind::Spaces);
    test_token!("\n\n\n", TokenKind::Newlines);
    test_token!(r"// line comment", TokenKind::LineComment);
    test_token!("123", TokenKind::Literal(Literal::Number));
    test_token_literal!(
        r#""terminated string literal""#,
        TokenKind::Literal(Literal::String),
        "\"terminated string literal\""
    );
    test_token_literal!(
        r#""unterminated string literal"#,
        TokenKind::Literal(Literal::String),
        "\"unterminated string literal"
    );
    test_token_literal!(
        r#""string \"escaped\" literal""#,
        TokenKind::Literal(Literal::String),
        "\"string \"escaped\" literal\""
    );
    test_token_literal!(
        r#""string with newline\nliteral""#,
        TokenKind::Literal(Literal::String),
        "\"string with newline\nliteral\""
    );
    test_token_literal!("'a'", TokenKind::Literal(Literal::Char), "'a'");
    test_token_literal!("'\\n'", TokenKind::Literal(Literal::Char), "'\n'");
    test_token!("/", TokenKind::Slash);
    test_token!(";", TokenKind::Semi);
    test_token!(",", TokenKind::Comma);
    test_token!("(", TokenKind::OpenParen);
    test_token!(")", TokenKind::CloseParen);
    test_token!("{", TokenKind::OpenBrace);
    test_token!("}", TokenKind::CloseBrace);
    test_token!(":", TokenKind::Colon);
    test_token!("=", TokenKind::Eq);
    test_token!("!", TokenKind::Not);
    test_token!("<", TokenKind::Lt);
    test_token!(">", TokenKind::Gt);
    test_token!("-", TokenKind::Minus);
    test_token!("&", TokenKind::And);
    test_token!("|", TokenKind::Or);
    test_token!("+", TokenKind::Plus);
    test_token!("*", TokenKind::Star);
    test_token!("^", TokenKind::Caret);
    test_token!("%", TokenKind::Percent);
}
