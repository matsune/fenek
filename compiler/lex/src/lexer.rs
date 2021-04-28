use super::scanner::*;
use super::token::*;
use error::{CompileError, LexerError, LitError, Result};
use pos::{Offset, SrcFile};

#[inline]
fn is_newline(c: char) -> bool {
    c == '\n' || c == '\r'
}
#[inline]
fn is_not_newline(c: char) -> bool {
    !is_newline(c)
}
#[inline]
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t'
}
#[inline]
fn is_underscore(c: char) -> bool {
    c == '_'
}
#[inline]
fn is_num(c: char) -> bool {
    matches!(c, '0'..='9')
}
#[inline]
fn is_num_or_underscore(c: char) -> bool {
    is_num(c) || is_underscore(c)
}
#[inline]
fn is_alphabet(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z')
}

// ident head
#[inline]
fn is_alphabet_or_underscore(c: char) -> bool {
    is_alphabet(c) || is_underscore(c)
}
#[inline]
fn is_alphanumeric(c: char) -> bool {
    is_alphabet(c) || is_num(c)
}

// ident body
#[inline]
fn is_alphanumeric_or_underscore(c: char) -> bool {
    is_alphanumeric(c) || is_underscore(c)
}

#[inline]
fn is_binary_digit(c: char) -> bool {
    c == '0' || c == '1'
}
#[inline]
fn is_octal_digit(c: char) -> bool {
    matches!(c, '0'..='7')
}
#[inline]
fn is_hex_digit(c: char) -> bool {
    matches!(c, '0'..='9' | 'A'..='F' | 'a'..='f' )
}

pub fn lex(src: &SrcFile) -> Result<Vec<Token>> {
    Lexer::new(&src).lex()
}

struct Lexer<'src> {
    src: &'src SrcFile,
    scanner: Scanner<'src>,
    offset: Offset,
}

impl<'src> Lexer<'src> {
    fn new(src: &'src SrcFile) -> Self {
        Lexer {
            src,
            scanner: Scanner::new(src.chars()),
            offset: 0,
        }
    }

    fn peek(&mut self) -> char {
        self.scanner.peek()
    }

    fn bump(&mut self) -> char {
        self.offset += 1;
        self.scanner.bump()
    }

    fn mk_tok<T: std::string::ToString>(&self, kind: TokenKind, raw: T, offset: Offset) -> Token {
        Token::new(kind, raw.to_string(), offset)
    }

    fn compile_error(&self, offset: Offset, err: LexerError) -> CompileError {
        CompileError::new(self.src.pos_from_offset(offset), Box::new(err))
    }

    fn lex(&mut self) -> Result<Vec<Token>> {
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

    fn scan(&mut self) -> Result<Token> {
        let offset = self.offset;
        let tok = match self.peek() {
            EOF => self.mk_tok(TokenKind::Eof, EOF, offset),
            c if is_whitespace(c) => {
                let raw = self.scan_while(is_whitespace);
                self.mk_tok(TokenKind::Spaces, raw, offset)
            }
            c if is_newline(c) => {
                let raw = self.scan_while(is_newline);
                self.mk_tok(TokenKind::Newlines, raw, offset)
            }
            '/' => {
                self.bump();
                match self.peek() {
                    '/' => {
                        let mut raw = self.scan_while(is_not_newline);
                        let c = self.bump();
                        if is_newline(c) {
                            raw.push(c);
                        }
                        self.mk_tok(TokenKind::LineComment, raw, offset)
                    }
                    _ => self.mk_tok(TokenKind::Slash, "/", offset),
                }
            }
            ';' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Semi, c, offset)
            }
            ',' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Comma, c, offset)
            }
            '(' => {
                let c = self.bump();
                self.mk_tok(TokenKind::LParen, c, offset)
            }
            ')' => {
                let c = self.bump();
                self.mk_tok(TokenKind::RParen, c, offset)
            }
            '{' => {
                let c = self.bump();
                self.mk_tok(TokenKind::LBrace, c, offset)
            }
            '}' => {
                let c = self.bump();
                self.mk_tok(TokenKind::RBrace, c, offset)
            }
            ':' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Colon, c, offset)
            }
            '=' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Eq, c, offset)
            }
            '!' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Not, c, offset)
            }
            '<' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Lt, c, offset)
            }
            '>' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Gt, c, offset)
            }
            '-' => {
                self.bump();
                if self.peek() == '>' {
                    self.bump();
                    self.mk_tok(TokenKind::Arrow, "->", offset)
                } else {
                    self.mk_tok(TokenKind::Minus, "-", offset)
                }
            }
            '&' => {
                let c = self.bump();
                self.mk_tok(TokenKind::And, c, offset)
            }
            '|' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Or, c, offset)
            }
            '+' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Plus, c, offset)
            }
            '*' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Star, c, offset)
            }
            '^' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Caret, c, offset)
            }
            '%' => {
                let c = self.bump();
                self.mk_tok(TokenKind::Percent, c, offset)
            }
            c if is_num(c) => self.scan_number()?,
            '"' => self.scan_string()?,
            // '\'' => self.scan_char()?,
            c if is_alphabet_or_underscore(c) => {
                let raw = self.scan_while(is_alphanumeric_or_underscore);
                let kind = match raw.as_str() {
                    "true" | "false" => TokenKind::Lit(LitKind::Bool),
                    _ => TokenKind::Ident,
                };
                self.mk_tok(kind, raw, offset)
            }
            c => return Err(self.compile_error(offset, LexerError::UnknwonToken(c))),
        };
        Result::Ok(tok)
    }

    fn bump_while<Pred, F>(&mut self, predicate: Pred, mut callback: F)
    where
        Pred: Fn(char) -> bool,
        F: FnMut(char),
    {
        while self.peek() != EOF && predicate(self.peek()) {
            callback(self.bump());
        }
    }

    fn scan_while<Pred>(&mut self, predicate: Pred) -> String
    where
        Pred: Fn(char) -> bool,
    {
        let mut raw = String::new();
        self.bump_while(predicate, |c| raw.push(c));
        raw
    }

    fn scan_number(&mut self) -> Result<Token> {
        let offset = self.offset;
        let c = self.bump();
        let mut raw = String::from(c);
        if c == '0' {
            match self.peek() {
                'b' | 'B' => {
                    raw.push(self.bump());
                    return self
                        .scan_binary_lit(&mut raw)
                        .map(|_| {
                            self.mk_tok(
                                TokenKind::Lit(LitKind::Int {
                                    base: IntBase::Binary,
                                }),
                                raw,
                                offset,
                            )
                        })
                        .map_err(|err| self.compile_error(offset, err.into()));
                }
                'o' | 'O' => {
                    raw.push(self.bump());
                    return self
                        .scan_octal_lit(&mut raw)
                        .map(|_| {
                            self.mk_tok(
                                TokenKind::Lit(LitKind::Int {
                                    base: IntBase::Octal,
                                }),
                                raw,
                                offset,
                            )
                        })
                        .map_err(|err| self.compile_error(offset, err.into()));
                }
                'x' | 'X' => {
                    raw.push(self.bump());
                    return self
                        .scan_hex_lit(&mut raw)
                        .map(|_| {
                            self.mk_tok(
                                TokenKind::Lit(LitKind::Int { base: IntBase::Hex }),
                                raw,
                                offset,
                            )
                        })
                        .map_err(|err| self.compile_error(offset, err.into()));
                }
                _ => {}
            }
        }
        self.bump_while(is_num_or_underscore, |c| raw.push(c));
        let tok = match self.peek() {
            '.' => {
                // floating point
                raw.push(self.bump());
                if !is_num(self.peek()) {
                    return Err(self.compile_error(offset, LitError::InvalidFloatLit.into()));
                }
                raw.push(self.bump());
                self.bump_while(is_num_or_underscore, |c| raw.push(c));
                self.mk_tok(TokenKind::Lit(LitKind::Float), raw, offset)
            }
            _ => self.mk_tok(
                TokenKind::Lit(LitKind::Int {
                    base: IntBase::Decimal,
                }),
                raw,
                offset,
            ),
        };
        Result::Ok(tok)
    }

    // This will be called after scanning binary prefix so
    // `raw` should be '0b' or '0B'.
    //
    // binary_lit       ::= "0" ("b" | "B") {"_"}* binary_digits
    // binary_digits    ::= binary_digit {binary_digit | "_"}*
    // binary_digit     ::= "0" | "1"
    fn scan_binary_lit(&mut self, raw: &mut String) -> std::result::Result<(), LitError> {
        while self.peek() == '_' {
            raw.push(self.bump());
        }
        if !is_binary_digit(self.peek()) {
            return Err(LitError::InvalidBinaryLit);
        }
        raw.push(self.bump());
        // parse while char is numeric or underscore
        // but only accepts binary digits.
        while is_num_or_underscore(self.peek()) {
            let c = self.peek();
            if is_binary_digit(c) || is_underscore(c) {
                raw.push(self.bump());
            } else {
                return Err(LitError::InvalidBinaryLit);
            }
        }
        Ok(())
    }

    // octal_lit       ::= "0" ("o" | "O") { "_" } octal_digits
    // octal_digits    ::= octal_digit { { "_" } octal_digit }
    // octal_digit     ::= "0" ... "7"
    fn scan_octal_lit(&mut self, raw: &mut String) -> std::result::Result<(), LitError> {
        while self.peek() == '_' {
            raw.push(self.bump());
        }
        if !is_octal_digit(self.peek()) {
            return Err(LitError::InvalidOctalLit);
        }
        raw.push(self.bump());
        while is_num_or_underscore(self.peek()) {
            let c = self.peek();
            if is_octal_digit(c) || is_underscore(c) {
                raw.push(self.bump());
            } else {
                return Err(LitError::InvalidOctalLit);
            }
        }
        Ok(())
    }

    // hex_lit       ::= "0" ("x" | "X") { "_" } hex_digits
    // hex_digits    ::= hex_digit { { "_" } hex_digit }
    // hex_digit     ::= "0" ... "9" | "A" ... "F" | "a" ... "f"
    fn scan_hex_lit(&mut self, raw: &mut String) -> std::result::Result<(), LitError> {
        while self.peek() == '_' {
            raw.push(self.bump());
        }
        if !is_hex_digit(self.peek()) {
            return Err(LitError::InvalidHexLit);
        }
        raw.push(self.bump());
        while is_hex_digit(self.peek()) || is_underscore(self.peek()) {
            raw.push(self.bump());
        }
        Ok(())
    }

    fn scan_string(&mut self) -> Result<Token> {
        let offset = self.offset;
        let mut raw = self.bump().to_string();
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
                            return Result::Err(self.compile_error(
                                self.offset,
                                LitError::UnknownCharEscape(c).into(),
                            ));
                        }
                    };
                    raw.push(escaped_c);
                }
                EOF => break,
                _ => raw.push(c),
            }
        }
        if terminated {
            Result::Ok(self.mk_tok(TokenKind::Lit(LitKind::String), raw, offset))
        } else {
            Result::Err(self.compile_error(offset, LitError::UnterminatedString(raw).into()))
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
