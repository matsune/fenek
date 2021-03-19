#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pos(pub u32, pub u32);

impl Default for Pos {
    fn default() -> Self {
        Pos(1, 1)
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl Pos {
    pub fn newline(&mut self) {
        self.0 += 1;
        self.1 = 1;
    }

    pub fn add_row(&mut self, row: u32) {
        self.1 += row;
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
}

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenKind::Unknown,
            literal: String::default(),
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, literal: String) -> Self {
        Token { kind, literal }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Unknown,
    Eof,
    Spaces,
    Newlines,
    LineComment,
    Slash,
    Semi,
    Comma,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Colon,
    Eq,
    Not,
    Lt,
    Gt,
    Minus,
    And,
    Or,
    Plus,
    Star,
    Caret,
    Percent,
}
