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
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LitKind {
    Int { base: IntBase },
    Float,
    Bool,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Eof,
    Spaces,
    Newlines,
    LineComment,
    Ident,
    Lit(LitKind),
    KwVar,
    KwFun,
    KwRet,
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
    Arrow,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenKind::Eof => "EOF",
            TokenKind::Spaces => "space",
            TokenKind::Newlines => "\\n",
            TokenKind::LineComment => "comment",
            TokenKind::Ident => "ident",
            TokenKind::Lit(kind) => match kind {
                LitKind::Int { base } => match base {
                    IntBase::Binary => "binary int literal",
                    IntBase::Octal => "octal int literal",
                    IntBase::Decimal => "int literal",
                    IntBase::Hex => "hex int literal",
                },
                LitKind::Float => "float literal",
                LitKind::Bool => "bool literal",
                LitKind::String => "string literal",
            },
            TokenKind::KwVar => "var",
            TokenKind::KwFun => "fun",
            TokenKind::KwRet => "ret",
            TokenKind::Slash => "/",
            TokenKind::Semi => ";",
            TokenKind::Comma => ",",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Colon => ":",
            TokenKind::Eq => "=",
            TokenKind::Not => "!",
            TokenKind::Lt => "<",
            TokenKind::Gt => ">",
            TokenKind::Minus => "-",
            TokenKind::And => "&",
            TokenKind::Or => "|",
            TokenKind::Plus => "+",
            TokenKind::Star => "*",
            TokenKind::Caret => "^",
            TokenKind::Percent => "%",
            TokenKind::Arrow => "->",
        };
        write!(f, "{}", s)
    }
}
