// pub use lex::IntBase;
// pub use syntax::ast;
// pub use syntax::parser;
// use typed_arena::Arena;

mod parser;

pub use parser::parse;
// mod lex;
// mod syntax;

// pub fn parse<'a>(
//     input: &str,
//     arena: &'a Arena<ast::AstNode<'a>>,
// ) -> Result<&'a ast::AstNode<'a>, error::CompileError> {
//     let tokens = lex::lex(input)?;
//     parser::parse(tokens.into(), arena)
// }
