use error::Result;
use lex::token;
use std::collections::VecDeque;

mod parser;

pub fn parse(tokens: VecDeque<token::Token>) -> Result<ast::Module> {
    parser::Parser::new(tokens).parse_module()
}
