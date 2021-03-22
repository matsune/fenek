use super::scanner::*;
use super::token::*;
use std::str::Chars;
use thiserror::Error;

#[cfg(test)]
mod tests;

#[derive(Error, Debug)]
pub enum LitError {
    // #[error("unterminated char literal: `{0}`")]
    // UnterminatedChar(String),
    #[error("unterminated string literal: `{0}`")]
    UnterminatedString(String),
    #[error("unknown character escape: `\\{0}`")]
    UnknownCharEscape(char),
    #[error("invalid binary literal")]
    InvalidBinaryLit,
    #[error("invalid octal literal")]
    InvalidOctalLit,
    #[error("invalid hex literal")]
    InvalidHexLit,
    #[error("invalid float literal")]
    InvalidFloatLit,
}

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("unknown token `{0}`")]
    UnknwonToken(char),
    #[error(transparent)]
    LitError(#[from] LitError),
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
fn is_num(c: char) -> bool {
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
    is_alphabet(c) || is_num(c)
}

fn is_binary_digit(c: char) -> bool {
    c == '0' || c == '1'
}

fn is_octal_digit(c: char) -> bool {
    match c {
        '0'..='7' => true,
        _ => false,
    }
}

fn is_hex_digit(c: char) -> bool {
    match c {
        '0'..='9' | 'A'..='F' | 'a'..='f' => true,
        _ => false,
    }
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

    pub fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut vec = Vec::new();
        loop {
            let tok = self.scan()?;
            if tok.kind == TokenKind::Eof {
                break;
            }
            vec.push(tok);
        }
        Ok(vec)
    }

    fn scan(&mut self) -> Result<Token, LexerError> {
        let c = self.bump();
        let tok = match c {
            EOF => Token::new(TokenKind::Eof, EOF.into()),
            c if is_whitespace(c) => self.scan_while(TokenKind::Spaces, c.into(), is_whitespace),
            c if is_newline(c) => self.scan_while(TokenKind::Newlines, c.into(), is_newline),
            '/' => match self.peek() {
                '/' => self.scan_while(TokenKind::LineComment, c.into(), |c| !is_newline(c)),
                _ => Token::new(TokenKind::Slash, c.into()),
            },
            ';' => Token::new(TokenKind::Semi, c.into()),
            ',' => Token::new(TokenKind::Comma, c.into()),
            '(' => Token::new(TokenKind::LParen, c.into()),
            ')' => Token::new(TokenKind::RParen, c.into()),
            '{' => Token::new(TokenKind::LBrace, c.into()),
            '}' => Token::new(TokenKind::RBrace, c.into()),
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
            c if is_num(c) => self.scan_number(c)?,
            '"' => self.scan_string()?,
            // '\'' => self.scan_char()?,
            c if is_alphabet(c) || c == '_' => {
                let mut tok = self.scan_while(TokenKind::Ident, c.into(), |c| {
                    is_alphanumeric(c) || c == '_'
                });
                tok.kind = match tok.raw.as_str() {
                    "true" => TokenKind::Lit(LitKind::Bool(true)),
                    "false" => TokenKind::Lit(LitKind::Bool(false)),
                    "var" => TokenKind::KwVar,
                    _ => TokenKind::Ident,
                };
                tok
            }
            _ => return Err(LexerError::UnknwonToken(c)),
        };
        Result::Ok(tok)
    }

    fn scan_while<F>(&mut self, kind: TokenKind, mut raw: String, predicate: F) -> Token
    where
        F: Fn(char) -> bool,
    {
        while self.peek() != EOF && predicate(self.peek()) {
            raw.push(self.bump());
        }
        Token::new(kind, raw)
    }

    fn scan_number(&mut self, first: char) -> Result<Token, LitError> {
        let mut raw = String::from(first);
        if first == '0' {
            match self.peek() {
                'b' | 'B' => {
                    raw.push(self.bump());
                    return self.scan_binary_lit(raw);
                }
                'o' | 'O' => {
                    raw.push(self.bump());
                    return self.scan_octal_lit(raw);
                }
                'x' | 'X' => {
                    raw.push(self.bump());
                    return self.scan_hex_lit(raw);
                }
                _ => {}
            }
        }
        while is_num(self.peek()) || self.peek() == '_' {
            raw.push(self.bump());
        }
        let tok = match self.peek() {
            '.' => {
                raw.push(self.bump());
                if !is_num(self.peek()) {
                    return Err(LitError::InvalidFloatLit);
                }
                raw.push(self.bump());
                while is_num(self.peek()) || self.peek() == '_' {
                    raw.push(self.bump());
                }
                Token::new(TokenKind::Lit(LitKind::Float), raw)
            }
            _ => Token::new(
                TokenKind::Lit(LitKind::Int {
                    base: IntBase::Decimal,
                }),
                raw,
            ),
        };
        Result::Ok(tok)
    }

    fn peek(&mut self) -> char {
        self.scanner.peek()
    }
    fn bump(&mut self) -> char {
        self.scanner.bump()
    }

    // binary_lit       ::= "0" ("b" | "B") { "_" } binary_digits
    // binary_digits    ::= binary_digit { { "_" } binary_digit }
    // binary_digit     ::= "0" | "1"
    fn scan_binary_lit(&mut self, mut raw: String) -> Result<Token, LitError> {
        while self.peek() == '_' {
            raw.push(self.bump());
        }
        if !is_binary_digit(self.peek()) {
            return Err(LitError::InvalidBinaryLit);
        }
        raw.push(self.bump());
        while self.peek() == '_' || is_num(self.peek()) {
            if self.peek() == '_' || is_binary_digit(self.peek()) {
                raw.push(self.bump());
            } else {
                return Err(LitError::InvalidBinaryLit);
            }
        }
        Ok(Token::new(
            TokenKind::Lit(LitKind::Int {
                base: IntBase::Binary,
            }),
            raw,
        ))
    }

    // octal_lit       ::= "0" ("o" | "O") { "_" } octal_digits
    // octal_digits    ::= octal_digit { { "_" } octal_digit }
    // octal_digit     ::= "0" ... "7"
    fn scan_octal_lit(&mut self, mut raw: String) -> Result<Token, LitError> {
        while self.peek() == '_' {
            raw.push(self.bump());
        }
        if !is_octal_digit(self.peek()) {
            return Err(LitError::InvalidOctalLit);
        }
        raw.push(self.bump());
        while self.peek() == '_' || is_num(self.peek()) {
            if self.peek() == '_' || is_octal_digit(self.peek()) {
                raw.push(self.bump());
            } else {
                return Err(LitError::InvalidOctalLit);
            }
        }
        Ok(Token::new(
            TokenKind::Lit(LitKind::Int {
                base: IntBase::Octal,
            }),
            raw,
        ))
    }

    // hex_lit       ::= "0" ("x" | "X") { "_" } hex_digits
    // hex_digits    ::= hex_digit { { "_" } hex_digit }
    // hex_digit     ::= "0" ... "9" | "A" ... "F" | "a" ... "f"
    fn scan_hex_lit(&mut self, mut raw: String) -> Result<Token, LitError> {
        while self.peek() == '_' {
            raw.push(self.bump());
        }
        if !is_hex_digit(self.peek()) {
            return Err(LitError::InvalidHexLit);
        }
        raw.push(self.bump());
        while self.peek() == '_' || is_hex_digit(self.peek()) {
            raw.push(self.bump());
        }
        Ok(Token::new(
            TokenKind::Lit(LitKind::Int { base: IntBase::Hex }),
            raw,
        ))
    }

    fn scan_string(&mut self) -> Result<Token, LitError> {
        let mut raw = "\"".to_string();
        let mut terminated = false;
        loop {
            let c = self.bump();
            match c {
                '"' => {
                    raw.push(c);
                    terminated = true;
                    break;
                }
                // '\' [ '\"' | common_escape ]
                '\\' => {
                    // common_escape : '\'
                    //               | 'n' | 'r' | 't' | '0'
                    //               | 'x' hex_digit 2
                    let escaped_c = match self.bump() {
                        '"' => '"',
                        '\\' => '\\',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '0' => '\0',
                        'x' => unimplemented!("hex_digit"),
                        c => return Result::Err(LitError::UnknownCharEscape(c)),
                    };
                    raw.push(escaped_c);
                }
                EOF => break,
                _ => raw.push(c),
            }
        }
        if terminated {
            Result::Ok(Token::new(TokenKind::Lit(LitKind::String), raw))
        } else {
            Result::Err(LitError::UnterminatedString(raw))
        }
    }

    // fn scan_char(&mut self) -> Result<Token, LitError> {
    //     let mut raw = "'".to_string();
    //     let mut terminated = false;
    //     loop {
    //         let c = self.bump();
    //         match c {
    //             '\'' => {
    //                 raw.push(c);
    //                 terminated = true;
    //                 break;
    //             }
    //             // '\' [ '\'' | common_escape ]
    //             '\\' => {
    //                 // common_escape : '\'
    //                 //               | 'n' | 'r' | 't' | '0'
    //                 //               | 'x' hex_digit 2
    //                 let escaped_c = match self.bump() {
    //                     '\'' => '\'',
    //                     '\\' => '\\',
    //                     'n' => '\n',
    //                     'r' => '\r',
    //                     't' => '\t',
    //                     '0' => '\0',
    //                     'x' => unimplemented!("hex_digit"),
    //                     c => return Result::Err(LitError::UnknownCharEscape(c)),
    //                 };
    //                 raw.push(escaped_c);
    //             }
    //             EOF => break,
    //             _ => raw.push(c),
    //         }
    //     }
    //     if terminated {
    //         Result::Ok(Token::new(TokenKind::Lit(LitKind::Char), raw))
    //     } else {
    //         Result::Err(LitError::UnterminatedChar(raw))
    //     }
    // }
}
