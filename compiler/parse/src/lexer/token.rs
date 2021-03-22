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
    pub raw: String,
}

impl Token {
    pub fn new(kind: TokenKind, raw: String) -> Self {
        Token { kind, raw }
    }

    pub fn is_lit(&self) -> bool {
        matches!(self.kind, TokenKind::Lit(_))
    }

    pub fn is_ident(&self) -> bool {
        matches!(self.kind, TokenKind::Ident)
    }

    pub fn is_spaces(&self) -> bool {
        matches!(self.kind, TokenKind::Spaces)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LitKind {
    Int { base: IntBase },
    Float,
    Bool(bool),
    // Char,
    String,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IntBase {
    Binary,
    Octal,
    Decimal,
    Hex,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Eof,
    Spaces,
    Newlines,
    LineComment,
    Ident,
    Lit(LitKind),
    KwVar,
    Slash,
    Semi,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
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
