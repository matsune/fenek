use error::Pos;

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub raw: String,
    pub pos: Pos,
}

impl Token {
    pub fn new(kind: TokenKind, raw: String, pos: Pos) -> Self {
        Token { kind, raw, pos }
    }

    pub fn is_lit(&self) -> bool {
        matches!(self.kind, TokenKind::Lit(_))
    }

    pub fn is_ident(&self) -> bool {
        matches!(self.kind, TokenKind::Ident)
    }

    pub fn is_spaces(&self) -> bool {
        matches!(self.kind, TokenKind::Spaces | TokenKind::Newlines)
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
    KwFun,
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
