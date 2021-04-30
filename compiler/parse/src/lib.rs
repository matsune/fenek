use error::Result;
use lex::token;
use pos::SrcFile;
use std::collections::VecDeque;

mod parser;

pub fn parse(src: &SrcFile, tokens: VecDeque<token::Token>) -> Result<ast::Module> {
    parser::Parser::new(src, tokens).parse_module()
}
