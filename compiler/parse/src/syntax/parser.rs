use super::ast::*;
use crate::lex::{IntBase, LexerError, Token, TokenKind};
use std::collections::{HashMap, VecDeque};
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
    #[error("constant {0} overflows int")]
    OverflowInt(String),
    #[error("invalid int literal {0}")]
    InvalidInt(String),
    #[error("invalid float literal {0}")]
    InvalidFloat(String),
    #[error("unclosed expr")]
    UnclosedParenExpr,
    #[error("invalid binary op {0}")]
    InvalidBinOp(String),
}

pub fn parse(tokens: VecDeque<Token>) -> Result<Vec<Stmt>, ParseError> {
    Parser::new(tokens).parse_stmts()
}

struct Parser {
    tokens: VecDeque<Token>,
    binop_map: HashMap<String, BinOp>,
    id: NodeId,
}

// https://github.com/rust-lang/rust/issues/22639
fn is_parse_int_overflow_error(e: std::num::ParseIntError) -> bool {
    let overflow_err = "4294967295000".parse::<u32>().err().unwrap();
    e == overflow_err
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Self {
        let mut binop_map = HashMap::new();
        binop_map.insert("+".to_string(), BinOp::new("+".to_string(), 10));
        binop_map.insert("-".to_string(), BinOp::new("-".to_string(), 10));
        binop_map.insert("*".to_string(), BinOp::new("*".to_string(), 20));
        binop_map.insert("/".to_string(), BinOp::new("/".to_string(), 20));
        Parser {
            tokens,
            binop_map,
            id: 0,
        }
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

    fn gen_id(&mut self) -> NodeId {
        self.id += 1;
        self.id
    }

    pub fn parse_stmts(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        loop {
            self.skip_spaces();
            if self.peek().is_none() {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
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
        Ok(VarDecl::new(
            self.gen_id(),
            Ident::new(self.gen_id(), name.raw),
            expr,
        ))
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
        let stmt = match tok.kind {
            TokenKind::KwVar => self.parse_var_decl()?.into(),
            _ => self.parse_expr()?.into(),
        };
        self.bump_if(|tok| tok.kind == TokenKind::Semi)
            .ok_or(ParseError::InvalidStmt)?;
        Ok(stmt)
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
                let kind = match kind {
                    crate::lex::LitKind::Int { base } => {
                        let n = match base {
                            IntBase::Binary => {
                                u64::from_str_radix(&tok.raw.replace("_", "")[2..], 2).map_err(
                                    |err| {
                                        if is_parse_int_overflow_error(err) {
                                            ParseError::OverflowInt(tok.raw.clone())
                                        } else {
                                            ParseError::InvalidInt(tok.raw.clone())
                                        }
                                    },
                                )?
                            }
                            IntBase::Octal => {
                                u64::from_str_radix(&tok.raw.replace("_", "")[2..], 8).map_err(
                                    |err| {
                                        if is_parse_int_overflow_error(err) {
                                            ParseError::OverflowInt(tok.raw.clone())
                                        } else {
                                            ParseError::InvalidInt(tok.raw.clone())
                                        }
                                    },
                                )?
                            }
                            IntBase::Decimal => {
                                tok.raw.replace("_", "").parse::<u64>().map_err(|err| {
                                    if is_parse_int_overflow_error(err) {
                                        ParseError::OverflowInt(tok.raw.clone())
                                    } else {
                                        ParseError::InvalidInt(tok.raw.clone())
                                    }
                                })?
                            }
                            IntBase::Hex => u64::from_str_radix(&tok.raw.replace("_", "")[2..], 16)
                                .map_err(|err| {
                                    if is_parse_int_overflow_error(err) {
                                        ParseError::OverflowInt(tok.raw.clone())
                                    } else {
                                        ParseError::InvalidInt(tok.raw.clone())
                                    }
                                })?,
                        };
                        if n > i64::MAX as u64 {
                            return Err(ParseError::OverflowInt(tok.raw));
                        }
                        LitKind::Int(n)
                    }
                    crate::lex::LitKind::Float => {
                        let f = tok
                            .raw
                            .replace("_", "")
                            .parse::<f64>()
                            .map_err(|_err| ParseError::InvalidFloat(tok.raw.clone()))?;
                        LitKind::Float(f)
                    }
                    crate::lex::LitKind::Bool(b) => LitKind::Bool(b),
                    crate::lex::LitKind::String => LitKind::String(tok.raw),
                };
                Expr::Lit(Lit::new(self.gen_id(), kind))
            }
            TokenKind::Ident => Ident::new(self.gen_id(), tok.raw).into(),
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

    fn peek_binop(&mut self) -> Option<&BinOp> {
        let mut idx = 0;
        let mut symbol = String::new();
        loop {
            match self.tokens.get(idx) {
                Some(tok)
                    if matches!(
                        tok.kind,
                        TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash
                    ) =>
                {
                    symbol.push_str(&tok.raw);
                    idx += 1;
                }
                _ => break,
            }
        }
        self.binop_map.get(&symbol)
    }

    fn parse_binop(&mut self) -> Option<&BinOp> {
        let mut symbol = String::new();
        while let Some(tok) = self.bump_if(|tok| {
            matches!(
                tok.kind,
                TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash
            )
        }) {
            symbol.push_str(&tok.raw);
        }
        self.binop_map.get(&symbol)
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
                Unary::new(self.gen_id(), unary_op, expr).into()
            }
            _ => self.parse_primary_expr()?,
        };
        self.skip_spaces();
        let bin_op = match self.peek_binop() {
            Some(op) if op.precedence >= last_prec => self.parse_binop().unwrap().clone(),
            _ => return Ok(lhs),
        };
        self.skip_spaces();
        let rhs = self.parse_expr_prec(bin_op.precedence)?;
        let mut lhs = Binary::new(self.gen_id(), bin_op, lhs, rhs).into();

        loop {
            self.skip_spaces();
            let bin_op = match self.peek_binop() {
                Some(op) if op.precedence >= last_prec => self.parse_binop().unwrap().clone(),
                _ => return Ok(lhs),
            };
            self.skip_spaces();
            let rhs = self.parse_expr_prec(bin_op.precedence)?;
            lhs = Binary::new(self.gen_id(), bin_op, lhs, rhs).into();
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
