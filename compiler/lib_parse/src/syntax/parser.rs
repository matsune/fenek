use super::ast::*;
use crate::lexer::{Lexer, LexerError, TokenKind};
use std::str::Chars;
use thiserror::Error;

#[cfg(test)]
mod tests;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    LexerError(#[from] LexerError),
    #[error("invalid expr")]
    InvalidExpr,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a mut Chars<'a>) -> Self {
        Parser {
            lexer: Lexer::new(source),
        }
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let tok = self.lexer.scan()?;
        match tok.kind {
            TokenKind::Lit(kind) => {
                let kind = LitKind::from(kind);
                Ok(Expr::new(ExprKind::Lit(Lit::new(kind, tok.raw))))
            }
            _ => unimplemented!(),
        }
    }
}
