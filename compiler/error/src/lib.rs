use pos::Pos;
use thiserror::Error;

#[derive(Error, Debug)]
pub struct CompileError {
    pos: Pos,
    err: Box<dyn std::error::Error>,
}

pub type Result<T, E = CompileError> = std::result::Result<T, E>;

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
    #[error("unterminated comment")]
    UnterminatedComment,
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
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error("expected {0}")]
    Expected(String),
    #[error("expected type name")]
    ExpectedTypeName,
    #[error("invalid var decl")]
    InvalidVarDecl,
    #[error("invalid expr")]
    InvalidExpr,
    #[error("unclosed expr")]
    UnclosedParenExpr,
    #[error("invalid binary op {0}")]
    InvalidBinOp(String),
    #[error("cannot use keyword {0}")]
    CannotUseKeyword(String),
    #[error("invalid arg name")]
    InvalidArgName,
    #[error("invalid type name")]
    InvalidTypeName,
    #[error("expected identifier, found keyword {0}")]
    FoundKeyword(String),
}

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("conflict types {0} and {1}")]
    ConflictTypes(String, String),
    #[error("unresolved type")]
    UnresolvedType,
    #[error("dereferencing non-pointer type {0}")]
    DerefNonPtrType(String),
    #[error("unknown type {0}")]
    UnknownType(String),
    #[error("already defined type {0}")]
    AlreadyDefinedType(String),
}

#[derive(Error, Debug)]
pub enum TypeCkError {
    #[error("already defined function `{0}`")]
    AlreadyDefinedFun(String),
    #[error("already defined variable `{0}`")]
    AlreadyDefinedVariable(String),
    #[error("invalid type")]
    InvalidType,
    #[error("undefined type `{0}`")]
    UndefinedType(String),
    #[error("undefined function `{0}`")]
    UndefinedFun(String),
    #[error("undefined variable `{0}`")]
    UndefinedVariable(String),
    #[error("invalid args count")]
    InvalidArgsCount,
    #[error("invalid return type")]
    InvalidReturnType,
    // #[error("invalid assign statement")]
    // InvalidAssign,
    #[error("found ret statement in the middle of block")]
    RetInMiddle,
    #[error("must be ret statement")]
    MustBeRetStmt,
    #[error("invalid binary types")]
    InvalidBinaryTypes,
    #[error("invalid unary types")]
    InvalidUnaryTypes,
    #[error("overflow {0}")]
    Overflow(String),
    #[error("constant {0} overflows {1}")]
    OverflowInt(String, String),
    #[error("invalid int literal {0}")]
    InvalidInt(String),
    #[error("invalid float literal {0}")]
    InvalidFloat(String),
    #[error("lvalue required")]
    LvalueRequired,
    #[error("assigning readonly")]
    AssigningReadonly,
    #[error("non basic type variable")]
    NonBasicVar,
    #[error("requires mutable")]
    RequiresMut,
    #[error("returning address of local variable")]
    ReturingLocalVar,
}
