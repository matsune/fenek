pub use pos::Pos;
use thiserror::Error;

mod pos;

#[derive(Error, Debug)]
pub struct CompileError {
    pos: Pos,
    err: Box<dyn std::error::Error>,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.pos, self.err)
    }
}

impl CompileError {
    pub fn new(pos: Pos, err: Box<dyn std::error::Error>) -> Self {
        Self { pos, err }
    }
}

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

#[derive(Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    LexerError(#[from] LexerError),

    #[error("invalid statement")]
    InvalidStmt,
    #[error("expected `{0}`")]
    Expected(&'static str),
    #[error("invalid var decl")]
    InvalidVarDecl,
    #[error("invalid expr")]
    InvalidExpr,
    #[error("constant {0} overflows int")]
    OverflowInt(String),
    #[error("invalid int literal {0}")]
    InvalidInt(String),
    #[error("invalid float literal {0}")]
    InvalidFloat(String),
    #[error("unclosed expr")]
    UnclosedParenExpr,
    #[error("invalid binary op {0}")]
    InvalidBinOp(String),
}

#[derive(Error, Debug)]
pub enum TypeCkError {
    #[error("already defined variable `{0}`")]
    AlreadyDefinedVariable(String),
    #[error("undefined variable `{0}`")]
    UndefinedVariable(String),
    #[error("invalid binary types")]
    InvalidBinaryTypes,
    #[error("invalid unary types")]
    InvalidUnaryTypes,
}
