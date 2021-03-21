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
    #[error("unclosed expr")]
    UnclosedParenExpr,
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
        if !tok.is_ident() && !tok.is_lit() && tok.kind != TokenKind::LParen {
            return Err(ParseError::InvalidExpr);
        }
        let tok = self.bump().unwrap();
        let expr = match tok.kind {
            TokenKind::Lit(kind) => {
                let kind = LitKind::from(kind);
                Expr::new_lit(kind, tok.raw.clone())
            }
            TokenKind::Ident => Expr::new_ident(tok.raw.clone()),
            TokenKind::LParen => {
                let expr = self.parse_expr()?;
                if self.peek().ok_or(ParseError::UnclosedParenExpr)?.kind != TokenKind::RParen {
                    return Err(ParseError::UnclosedParenExpr);
                }
                self.bump();
                expr
            }
            _ => unreachable!(),
        };
        Ok(expr)
    }

    fn parse_expr_prec(&mut self, last_prec: u8) -> Result<Expr, ParseError> {
        let tok = self.peek().ok_or(ParseError::InvalidExpr)?;
        let lhs = match tok.kind {
            TokenKind::Not | TokenKind::Plus | TokenKind::Minus => {
                let unary_op = match self.bump().unwrap().kind {
                    TokenKind::Not => UnaryOp::Not,
                    TokenKind::Plus => UnaryOp::Add,
                    TokenKind::Minus => UnaryOp::Sub,
                    _ => unreachable!(),
                };
                let expr = self.parse_primary_expr()?;
                Expr::new_unary(unary_op, expr)
            }
            _ => self.parse_primary_expr()?,
        };
        self.skip_spaces();
        let bin_op_kind = match self.peek() {
            Some(tok)
                if tok.kind == TokenKind::Plus
                    || tok.kind == TokenKind::Minus
                    || tok.kind == TokenKind::Star
                    || tok.kind == TokenKind::Slash =>
            {
                match tok.kind {
                    TokenKind::Plus => BinOp::Add,
                    TokenKind::Minus => BinOp::Sub,
                    TokenKind::Star => BinOp::Mul,
                    TokenKind::Slash => BinOp::Div,
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
                        TokenKind::Plus => BinOp::Add,
                        TokenKind::Minus => BinOp::Sub,
                        TokenKind::Star => BinOp::Mul,
                        TokenKind::Slash => BinOp::Div,
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
