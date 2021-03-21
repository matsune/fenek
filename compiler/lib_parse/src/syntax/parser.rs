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
        self.parse_expr_prec(0)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, ParseError> {
        let tok = self.peek().ok_or(ParseError::InvalidExpr)?;
        if !tok.is_ident() && !tok.is_lit() {
            return Err(ParseError::InvalidExpr);
        }
        let tok = self.bump().unwrap();
        let expr = match tok.kind {
            TokenKind::Lit(kind) => {
                let kind = LitKind::from(kind);
                Expr::new_lit(kind, tok.raw.clone())
            }
            TokenKind::Ident => Expr::new_ident(tok.raw.clone()),
            _ => unimplemented!(),
        };
        Ok(expr)
    }

    fn parse_expr_prec(&mut self, last_prec: u8) -> Result<Expr, ParseError> {
        let lhs = self.parse_primary_expr()?;
        self.skip_spaces();
        let bin_op_kind = match self.peek() {
            Some(tok)
                if tok.kind == TokenKind::Plus
                    || tok.kind == TokenKind::Minus
                    || tok.kind == TokenKind::Star
                    || tok.kind == TokenKind::Slash =>
            {
                match tok.kind {
                    TokenKind::Plus => BinOpKind::Add,
                    TokenKind::Minus => BinOpKind::Sub,
                    TokenKind::Star => BinOpKind::Mul,
                    TokenKind::Slash => BinOpKind::Div,
                    _ => return Ok(lhs),
                }
            }
            _ => return Ok(lhs),
        };
        if bin_op_kind.precedence() < last_prec {
            return Ok(lhs);
        }
        self.bump();
        self.skip_spaces();
        let rhs = self.parse_expr_prec(bin_op_kind.precedence())?;
        let mut lhs = Expr::new_binary(bin_op_kind, lhs, rhs);

        loop {
            self.skip_spaces();
            let bin_op_kind = match self.peek() {
                Some(tok)
                    if tok.kind == TokenKind::Plus
                        || tok.kind == TokenKind::Minus
                        || tok.kind == TokenKind::Star
                        || tok.kind == TokenKind::Slash =>
                {
                    match tok.kind {
                        TokenKind::Plus => BinOpKind::Add,
                        TokenKind::Minus => BinOpKind::Sub,
                        TokenKind::Star => BinOpKind::Mul,
                        TokenKind::Slash => BinOpKind::Div,
                        _ => return Ok(lhs),
                    }
                }
                _ => return Ok(lhs),
            };
            if bin_op_kind.precedence() < last_prec {
                return Ok(lhs);
            }
            self.bump();
            self.skip_spaces();
            let rhs = self.parse_expr_prec(bin_op_kind.precedence())?;
            lhs = Expr::new_binary(bin_op_kind, lhs, rhs);
        }
    }

    fn skip_spaces(&mut self) {
        match self.peek() {
            Some(peek) if peek.is_spaces() => {
                self.bump();
            }
            _ => {}
        }
    }
}
