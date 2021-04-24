use super::scanner::*;
use super::token::*;
use error::{CompileError, LexerError, LitError, Pos};
use std::convert::TryInto;
use std::str::Chars;

#[cfg(test)]
mod tests;

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
    matches!(c, '0'..='9')
}

fn is_alphabet(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z')
}

fn is_alphanumeric(c: char) -> bool {
    is_alphabet(c) || is_num(c)
}

fn is_binary_digit(c: char) -> bool {
    c == '0' || c == '1'
}

fn is_octal_digit(c: char) -> bool {
    matches!(c, '0'..='7')
}

fn is_hex_digit(c: char) -> bool {
    matches!(c, '0'..='9' | 'A'..='F' | 'a'..='f' )
}

pub fn lex(source: &str) -> Result<Vec<Token>, CompileError> {
    let mut chars = source.chars();
    Lexer::new(&mut chars).lex()
}

struct Lexer<'a> {
    scanner: Scanner<'a>,
    begin: Pos,
    end: Pos,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a mut Chars<'a>) -> Self {
        Lexer {
            scanner: Scanner::new(source),
            begin: Pos::default(),
            end: Pos::default(),
        }
    }

    fn lex(&mut self) -> Result<Vec<Token>, CompileError> {
        let mut vec = Vec::new();
        loop {
            self.begin = self.end;
            let tok = self
                .scan()
                .map_err(|err| CompileError::new(self.begin, Box::new(err)))?;
            self.begin = self.end;
            if tok.kind == TokenKind::Eof {
                break;
            }
            vec.push(tok);
        }
        Ok(vec)
    }

    fn mk_tok<T: std::string::ToString>(&self, kind: TokenKind, raw: T) -> Token {
        Token::new(kind, raw.to_string(), self.begin)
    }

    fn scan(&mut self) -> Result<Token, LexerError> {
        let c = self.bump();
        let tok = match c {
            EOF => self.mk_tok(TokenKind::Eof, EOF),
            c if is_whitespace(c) => self.scan_while(TokenKind::Spaces, c.into(), is_whitespace),
            c if is_newline(c) => self.scan_while(TokenKind::Newlines, c.into(), is_newline),
            '/' => match self.peek() {
                '/' => {
                    let mut tok =
                        self.scan_while(TokenKind::LineComment, c.into(), |c| !is_newline(c));
                    let c = self.bump();
                    if is_newline(c) {
                        tok.raw.push(c);
                    }
                    tok
                }
                _ => self.mk_tok(TokenKind::Slash, c),
            },
            ';' => self.mk_tok(TokenKind::Semi, c),
            ',' => self.mk_tok(TokenKind::Comma, c),
            '(' => self.mk_tok(TokenKind::LParen, c),
            ')' => self.mk_tok(TokenKind::RParen, c),
            '{' => self.mk_tok(TokenKind::LBrace, c),
            '}' => self.mk_tok(TokenKind::RBrace, c),
            ':' => self.mk_tok(TokenKind::Colon, c),
            '=' => self.mk_tok(TokenKind::Eq, c),
            '!' => self.mk_tok(TokenKind::Not, c),
            '<' => self.mk_tok(TokenKind::Lt, c),
            '>' => self.mk_tok(TokenKind::Gt, c),
            '-' => {
                if self.peek() == '>' {
                    self.bump();
                    self.mk_tok(TokenKind::Arrow, "->")
                } else {
                    self.mk_tok(TokenKind::Minus, c)
                }
            }
            '&' => self.mk_tok(TokenKind::And, c),
            '|' => self.mk_tok(TokenKind::Or, c),
            '+' => self.mk_tok(TokenKind::Plus, c),
            '*' => self.mk_tok(TokenKind::Star, c),
            '^' => self.mk_tok(TokenKind::Caret, c),
            '%' => self.mk_tok(TokenKind::Percent, c),
            c if is_num(c) => self.scan_number(c)?,
            '"' => self.scan_string()?,
            // '\'' => self.scan_char()?,
            c if is_alphabet(c) || c == '_' => {
                let mut tok = self.scan_while(TokenKind::Ident, c.into(), |c| {
                    is_alphanumeric(c) || c == '_'
                });
                tok.kind = match tok.raw.as_str() {
                    "true" | "false" => TokenKind::Lit(LitKind::Bool),
                    "var" => TokenKind::KwVar,
                    "fun" => TokenKind::KwFun,
                    "ret" => TokenKind::KwRet,
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
        self.mk_tok(kind, raw)
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
                self.mk_tok(TokenKind::Lit(LitKind::Float), raw)
            }
            _ => self.mk_tok(
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
        let c = self.scanner.bump();
        if is_newline(c) {
            self.end.newline();
        } else {
            self.end.add_row(c.len_utf16().try_into().unwrap());
        }
        c
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
        Ok(self.mk_tok(
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
        Ok(self.mk_tok(
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
        Ok(self.mk_tok(TokenKind::Lit(LitKind::Int { base: IntBase::Hex }), raw))
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
                        c => {
                            return Result::Err(LitError::UnknownCharEscape(c));
                        }
                    };
                    raw.push(escaped_c);
                }
                EOF => break,
                _ => raw.push(c),
            }
        }
        if terminated {
            Result::Ok(self.mk_tok(TokenKind::Lit(LitKind::String), raw))
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
    //         Result::Ok(self.mk_tok(TokenKind::Lit(LitKind::Char), raw))
    //     } else {
    //         Result::Err(LitError::UnterminatedChar(raw))
    //     }
    // }
}
