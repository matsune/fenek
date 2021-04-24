use super::ast::*;
use crate::lex::{IntBase, Token, TokenKind};
use error::{CompileError, ParseError, Pos};
use std::collections::{HashMap, VecDeque};

#[cfg(test)]
mod tests;

type Result<T> = std::result::Result<T, CompileError>;

pub fn parse(tokens: VecDeque<Token>) -> Result<Fun> {
    Parser::new(tokens).parse_fun()
}

struct Parser {
    tokens: VecDeque<Token>,
    binop_map: HashMap<String, BinOp>,
    id: NodeId,
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

    fn bump(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    fn gen_id(&mut self) -> NodeId {
        self.id += 1;
        self.id
    }

    fn current_pos(&self) -> Pos {
        if let Some(tok) = self.peek() {
            tok.pos
        } else {
            Pos::default()
        }
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

    fn skip_spaces(&mut self) {
        loop {
            match self.peek() {
                Some(peek)
                    if matches!(
                        peek.kind,
                        TokenKind::Spaces | TokenKind::Newlines | TokenKind::LineComment
                    ) =>
                {
                    self.bump();
                }
                _ => return,
            }
        }
    }

    fn expected_err<T: std::string::ToString>(&self, expected: T) -> CompileError {
        CompileError::new(
            self.current_pos(),
            Box::new(ParseError::Expected(expected.to_string())),
        )
    }

    fn compile_error(&self, error: ParseError) -> CompileError {
        CompileError::new(self.current_pos(), Box::new(error))
    }

    fn is_next_kind(&self, kind: TokenKind) -> bool {
        matches!(self.peek(), Some(tok) if tok.kind == kind)
    }

    fn expect_kind(&mut self, kind: TokenKind) -> Result<Token> {
        let expect = kind.to_string();
        self.bump_if(|tok| tok.kind == kind)
            .ok_or_else(|| self.expected_err(expect.as_str()))
    }

    fn new_ident(&mut self, ident: Token) -> Ident {
        Ident::new(self.gen_id(), ident.raw, ident.pos)
    }

    fn parse_ident(&mut self) -> Result<Ident> {
        let ident = self.expect_kind(TokenKind::Ident)?;
        Ok(self.new_ident(ident))
    }

    pub fn parse_fun(&mut self) -> Result<Fun> {
        let kw_fun = self.expect_kind(TokenKind::KwFun)?;
        let pos = kw_fun.pos;
        self.skip_spaces();
        let name = self.parse_ident()?;
        self.skip_spaces();
        let args = self.parse_fun_args()?;
        self.skip_spaces();
        let ret_ty = match self.peek() {
            Some(tok) if tok.kind == TokenKind::LBrace => None,
            _ => {
                // has return type
                self.expect_kind(TokenKind::Arrow)?;
                self.skip_spaces();
                let ret_ty = self.parse_ident()?;
                self.skip_spaces();
                Some(ret_ty)
            }
        };
        let block = self.parse_block()?;
        Ok(Fun::new(self.gen_id(), name, args, ret_ty, block, pos))
    }

    fn parse_block(&mut self) -> Result<Block> {
        self.expect_kind(TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        loop {
            self.skip_spaces();
            match self.peek() {
                None => break,
                Some(tok) => match tok.kind {
                    TokenKind::Semi => {
                        // empty stmt
                        self.bump();
                        continue;
                    }
                    TokenKind::RBrace => break,
                    _ => {}
                },
            }
            stmts.push(self.parse_stmt()?);
        }

        self.expect_kind(TokenKind::RBrace)?;
        Ok(Block::new(self.gen_id(), stmts))
    }

    /// fun_args    ::= '(' <args>* ')'
    /// args        ::= <ident> ':' <ident> (',' <args>)?
    fn parse_fun_args(&mut self) -> Result<FunArgs> {
        self.expect_kind(TokenKind::LParen)?;
        self.skip_spaces();

        let mut args = Vec::new();
        if self.is_next_kind(TokenKind::RParen) {
            // empty args
            self.bump();
            return Ok(args);
        }

        loop {
            let arg_name = self.parse_ident()?;
            self.skip_spaces();
            self.expect_kind(TokenKind::Colon)?;
            self.skip_spaces();
            let arg_ty = self.parse_ident()?;
            self.skip_spaces();
            args.push(FunArg::new(self.gen_id(), arg_name, arg_ty));
            if self.is_next_kind(TokenKind::Comma) {
                // has next arg
                self.bump();
                self.skip_spaces();
            } else {
                break;
            }
        }

        self.expect_kind(TokenKind::RParen)?;
        Ok(args)
    }

    pub fn parse_var_decl(&mut self) -> Result<VarDecl> {
        let pos = self.expect_kind(TokenKind::KwVar)?.pos;
        self.skip_spaces();
        let name = self.parse_ident()?;
        self.skip_spaces();
        self.expect_kind(TokenKind::Eq)?;
        self.skip_spaces();
        let expr = self.parse_expr()?;
        Ok(VarDecl::new(self.gen_id(), name, expr, pos))
    }

    pub fn parse_ret(&mut self) -> Result<Ret> {
        let ret = self.expect_kind(TokenKind::KwRet)?;
        self.skip_spaces();
        if self.is_next_kind(TokenKind::Semi) {
            return Ok(Ret::new(self.gen_id(), None, ret.pos));
        }
        let expr = self.parse_expr()?;
        Ok(Ret::new(self.gen_id(), Some(expr), ret.pos))
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt> {
        let tok = self
            .peek()
            .ok_or_else(|| self.compile_error(ParseError::UnexpectedEof))?;
        let stmt = match tok.kind {
            TokenKind::KwVar => self.parse_var_decl()?.into(),
            TokenKind::KwRet => self.parse_ret()?.into(),
            _ => self.parse_expr()?.into(),
        };
        self.expect_kind(TokenKind::Semi)?;
        Ok(stmt)
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_expr_prec(0)
    }

    fn parse_primary_expr(&mut self) -> Result<Expr> {
        let tok = self
            .bump_if(|tok| {
                matches!(
                    tok.kind,
                    TokenKind::Ident | TokenKind::Lit(_) | TokenKind::LParen
                )
            })
            .ok_or_else(|| self.compile_error(ParseError::InvalidExpr))?;
        let expr = match tok.kind {
            TokenKind::Lit(kind) => {
                let kind = match kind {
                    crate::lex::LitKind::Int { base } => {
                        // if n > i64::MAX as u64 {
                        //     return Err(self.compile_error(ParseError::OverflowInt(tok.raw)));
                        // }
                        LitKind::Int(base)
                    }
                    crate::lex::LitKind::Float => {
                        // let f = tok.raw.replace("_", "").parse::<f64>().map_err(|_| {
                        //     self.compile_error(ParseError::InvalidFloat(tok.raw.clone()))
                        // })?;
                        LitKind::Float //(f)
                    }
                    crate::lex::LitKind::Bool => LitKind::Bool,
                    crate::lex::LitKind::String => LitKind::String,
                };
                Expr::Lit(Lit::new(self.gen_id(), kind, tok.raw, tok.pos))
            }
            TokenKind::Ident => Ident::new(self.gen_id(), tok.raw, tok.pos).into(),
            TokenKind::LParen => {
                let expr = self.parse_expr()?;
                if !self.is_next_kind(TokenKind::RParen) {
                    return Err(self.compile_error(ParseError::UnclosedParenExpr));
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

    fn parse_expr_prec(&mut self, last_prec: u8) -> Result<Expr> {
        let tok = self
            .peek()
            .ok_or_else(|| self.compile_error(ParseError::UnexpectedEof))?;
        let lhs = match tok.kind {
            TokenKind::Not | TokenKind::Plus | TokenKind::Minus => {
                let pos = tok.pos;
                let unary_op = match self.bump().unwrap().kind {
                    TokenKind::Not => UnaryOp::Not,
                    TokenKind::Plus => UnaryOp::Add,
                    TokenKind::Minus => UnaryOp::Sub,
                    _ => unreachable!(),
                };
                self.skip_spaces();
                let expr = self.parse_expr()?;
                Unary::new(self.gen_id(), unary_op, expr, pos).into()
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
}
