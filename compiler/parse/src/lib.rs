mod lex;
mod syntax;

pub use syntax::ast;

pub fn parse(input: &str) -> Result<syntax::ast::Fun, error::CompileError> {
    let tokens = lex::lex(input)?;
    syntax::parser::parse(tokens.into())
}
