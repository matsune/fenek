mod lex;
mod syntax;

pub use syntax::ast;

pub fn parse(input: &str) -> Result<Vec<syntax::ast::Stmt>, syntax::parser::ParseError> {
    let tokens = lex::lex(input)?;
    syntax::parser::parse(tokens.into())
}
