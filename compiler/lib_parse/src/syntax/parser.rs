use super::ast::*;
use crate::lexer::{Lexer, LexerError, Token, TokenKind};
use std::collections::VecDeque;
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

pub struct Parser {
    tokens: VecDeque<Token>,
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Self {
        Parser { tokens }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }

    fn peek_mut(&mut self) -> Option<&mut Token> {
        self.tokens.front_mut()
    }

    fn bump(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let tok = self.peek().ok_or(ParseError::InvalidExpr)?;
        let expr1 = match tok.kind {
            TokenKind::Lit(kind) => {
                let kind = LitKind::from(kind);
                Expr::new_lit(kind, tok.raw.clone())
            }
            TokenKind::Ident => Expr::new_ident(tok.raw.clone()),
            _ => unimplemented!(),
        };
        Ok(expr1)
    }
}
