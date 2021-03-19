use super::scanner::{Scanner, EOF};
use super::token::{Literal, Token, TokenKind};
use std::str::Chars;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum LiteralError {
    #[error("unterminated char literal: `{0}`")]
    UnterminatedChar(String),
    #[error("unterminated string literal: `{0}`")]
    UnterminatedString(String),
    #[error("unknown character escape: `\\{0}`")]
    UnknownCharEscape(char),
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error(transparent)]
    LiteralError(#[from] LiteralError),
}

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

fn is_alphabet(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' => true,
        _ => false,
    }
}

fn is_alphanumeric(c: char) -> bool {
    is_alphabet(c) || is_dec_digit(c)
}

pub struct Lexer<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a mut Chars<'a>) -> Self {
        Lexer {
            scanner: Scanner::new(source),
        }
    }

    pub fn scan(&mut self) -> Result<Token, LexerError> {
        let c = self.scanner.bump();
        let tok = match c {
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
            '"' => self.scan_string()?,
            '\'' => self.scan_char()?,
            c if is_alphabet(c) || c == '_' => {
                let mut tok = self.scan_while(TokenKind::Ident, c.into(), |c| {
                    is_alphanumeric(c) || c == '_'
                });
                tok.kind = match tok.literal.as_str() {
                    "true" => TokenKind::Literal(Literal::Bool(true)),
                    "false" => TokenKind::Literal(Literal::Bool(false)),
                    _ => TokenKind::Ident,
                };
                tok
            }
            _ => Token::new(TokenKind::Unknown, c.into()),
        };
        Result::Ok(tok)
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

    fn scan_string(&mut self) -> Result<Token, LiteralError> {
        let mut literal = "\"".to_string();
        let mut terminated = false;
        loop {
            let c = self.scanner.bump();
            match c {
                '"' => {
                    literal.push(c);
                    terminated = true;
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
                        c => return Result::Err(LiteralError::UnknownCharEscape(c)),
                    };
                    literal.push(escaped_c);
                }
                EOF => break,
                _ => literal.push(c),
            }
        }
        if terminated {
            Result::Ok(Token::new(TokenKind::Literal(Literal::String), literal))
        } else {
            Result::Err(LiteralError::UnterminatedString(literal))
        }
    }

    fn scan_char(&mut self) -> Result<Token, LiteralError> {
        let mut literal = "'".to_string();
        let mut terminated = false;
        loop {
            let c = self.scanner.bump();
            match c {
                '\'' => {
                    literal.push(c);
                    terminated = true;
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
                        c => return Result::Err(LiteralError::UnknownCharEscape(c)),
                    };
                    literal.push(escaped_c);
                }
                EOF => break,
                _ => literal.push(c),
            }
        }
        if terminated {
            Result::Ok(Token::new(TokenKind::Literal(Literal::Char), literal))
        } else {
            Result::Err(LiteralError::UnterminatedChar(literal))
        }
    }
}
