use super::ast::*;
use crate::lexer::*;
use std::collections::VecDeque;
use std::str::Chars;
use thiserror::Error;

#[cfg(test)]
mod tests;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error(transparent)]
    LexerError(#[from] LexerError),

    #[error("invalid statement")]
    InvalidStmt,
    #[error("expected {0}")]
    Expected(&'static str),
    #[error("invalid var decl")]
    InvalidVarDecl,
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

    pub fn parse_var_decl(&mut self) -> Result<VarDecl, ParseError> {
        self.bump_if(|tok| tok.kind == TokenKind::KwVar)
            .ok_or(ParseError::Expected("`var`"))?;
        self.bump_if(|tok| tok.is_spaces())
            .ok_or(ParseError::Expected("spaces"))?;
        let name = self
            .bump_if(|tok| tok.is_ident())
            .ok_or(ParseError::Expected("ident"))?;
        self.skip_spaces();
        self.bump_if(|tok| tok.kind == TokenKind::Eq)
            .ok_or(ParseError::Expected("`=`"))?;
        self.skip_spaces();
        let expr = self.parse_expr()?;
        Ok(VarDecl::new(Ident::new(name.raw), expr))
    }

    fn bump_if<Fn>(&mut self, pred: Fn) -> Option<Token>
    where
        Fn: FnOnce(&Token) -> bool,
    {
        if let Some(tok) = self.peek() {
            if pred(tok) {
                return self.bump();
            }
        }
        None
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let tok = self.peek().ok_or(ParseError::InvalidStmt)?;
        let kind = match tok.kind {
            TokenKind::KwVar => StmtKind::VarDecl(self.parse_var_decl()?),
            _ => StmtKind::Expr(self.parse_expr()?),
        };
        self.bump_if(|tok| tok.kind == TokenKind::Semi)
            .ok_or(ParseError::InvalidStmt)?;
        Ok(Stmt::new(kind))
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_expr_prec(0)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, ParseError> {
        let tok = self
            .bump_if(|tok| tok.is_ident() || tok.is_lit() || tok.kind == TokenKind::LParen)
            .ok_or(ParseError::InvalidExpr)?;
        let expr = match tok.kind {
            TokenKind::Lit(kind) => {
                let lit = match kind {
                    LitKind::Int { base } => match base {
                        IntBase::Binary => {
                            let n = u64::from_str_radix(&tok.raw.replace("_", "")[2..], 2)
                                .map_err(|_err| ParseError::InvalidExpr)?;
                            Lit::Int(n)
                        }
                        IntBase::Octal => {
                            let n = u64::from_str_radix(&tok.raw.replace("_", "")[2..], 8)
                                .map_err(|_err| ParseError::InvalidExpr)?;
                            Lit::Int(n)
                        }
                        IntBase::Decimal => {
                            let n = tok
                                .raw
                                .replace("_", "")
                                .parse::<u64>()
                                .map_err(|_err| ParseError::InvalidExpr)?;
                            Lit::Int(n)
                        }
                        IntBase::Hex => {
                            let n = u64::from_str_radix(&tok.raw.replace("_", "")[2..], 16)
                                .map_err(|_err| ParseError::InvalidExpr)?;
                            Lit::Int(n)
                        }
                    },
                    LitKind::Float => {
                        let f = tok
                            .raw
                            .replace("_", "")
                            .parse::<f64>()
                            .map_err(|_err| ParseError::InvalidExpr)?;
                        Lit::Float(f)
                    }
                    LitKind::Bool(b) => Lit::Bool(b),
                    LitKind::String => Lit::String(tok.raw),
                };
                Expr::new_lit(lit)
            }
            TokenKind::Ident => Expr::new_ident(tok.raw),
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
