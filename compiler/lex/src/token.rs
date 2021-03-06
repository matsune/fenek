use pos::Pos;
use serde::{Serialize, Serializer};

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub raw: String,
    pub pos: Pos,
}

impl Token {
    pub fn try_as_keyword(&self) -> Option<Keyword> {
        Keyword::try_from(&self.raw)
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize)]
pub enum LitKind {
    Int { base: IntBase },
    Float,
    Bool,
    // Char,
    String,
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize)]
pub enum IntBase {
    Binary,
    Octal,
    Decimal,
    Hex,
}

impl Into<u32> for IntBase {
    fn into(self) -> u32 {
        match self {
            Self::Binary => 2,
            Self::Octal => 8,
            Self::Decimal => 10,
            Self::Hex => 16,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Eof,
    Spaces,
    Newlines,
    LineComment,
    BlockComment,
    Ident,
    Lit(LitKind),
    // /
    Slash,
    // ;
    Semi,
    // ,
    Comma,
    // (
    LParen,
    // )
    RParen,
    // {
    LBrace,
    // }
    RBrace,
    // :
    Colon,
    // =
    Eq,
    // !
    Not,
    // <
    Lt,
    // >
    Gt,
    // <=
    Le,
    // >=
    Ge,
    // -
    Minus,
    // &
    And,
    // |
    Or,
    // +
    Plus,
    // *
    Star,
    // ^
    Caret,
    // %
    Percent,
    // ->
    Arrow,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TokenKind::Eof => "EOF",
            TokenKind::Spaces => "space",
            TokenKind::Newlines => "\\n",
            TokenKind::BlockComment => "block comment",
            TokenKind::LineComment => "line comment",
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
            TokenKind::Le => "<=",
            TokenKind::Ge => ">=",
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Keyword {
    Mut,
    Let,
    Ret,
    Fun,
    If,
    Else,
    Struct,
    Null,
}

impl std::string::ToString for Keyword {
    fn to_string(&self) -> String {
        match self {
            Self::Mut => "mut",
            Self::Let => "let",
            Self::Ret => "ret",
            Self::Fun => "fun",
            Self::If => "if",
            Self::Else => "else",
            Self::Struct => "struct",
            Self::Null => "null",
        }
        .to_owned()
    }
}

impl Keyword {
    pub fn try_from(s: &str) -> Option<Self> {
        match s {
            "mut" => Some(Self::Mut),
            "let" => Some(Self::Let),
            "ret" => Some(Self::Ret),
            "fun" => Some(Self::Fun),
            "if" => Some(Self::If),
            "else" => Some(Self::Else),
            "struct" => Some(Self::Struct),
            "null" => Some(Self::Null),
            _ => None,
        }
    }
}

impl Serialize for Keyword {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}
