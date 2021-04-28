use ast::*;
use error::{CompileError, ParseError, Result};
use lex::token;
use span::{Offset, Pos, SrcFile};
use std::collections::{HashMap, VecDeque};

pub fn parse(src: &SrcFile, tokens: VecDeque<token::Token>) -> Result<Stmt> {
    Parser::new(src, tokens).parse_stmt() //.parse_fun()
}

struct Parser<'src> {
    src: &'src SrcFile,
    tokens: VecDeque<token::Token>,
    node_id: NodeId,
    last_offset: Offset,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src SrcFile, tokens: VecDeque<token::Token>) -> Self {
        Parser {
            src,
            tokens,
            node_id: 0,
            last_offset: 0,
        }
    }

    fn peek(&self) -> Option<&token::Token> {
        self.tokens.front()
    }

    fn bump(&mut self) -> Option<token::Token> {
        let tok = self.tokens.pop_front();
        if let Some(tok) = &tok {
            self.last_offset = tok.offset + tok.raw.chars().count() as Offset;
        }
        tok
    }

    fn gen_id(&mut self) -> NodeId {
        self.node_id += 1;
        self.node_id
    }

    fn current_pos(&self) -> Pos {
        let offset = self
            .peek()
            .map(|tok| tok.offset)
            .unwrap_or(self.last_offset);
        self.src.pos_from_offset(offset)
    }

    fn bump_if<Fn>(&mut self, pred: Fn) -> Option<token::Token>
    where
        Fn: FnOnce(&token::Token) -> bool,
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
                        token::TokenKind::Spaces
                            | token::TokenKind::Newlines
                            | token::TokenKind::LineComment
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
        CompileError::new(self.current_pos(), error.into())
    }

    fn is_next_kind(&self, kind: token::TokenKind) -> bool {
        matches!(self.peek(), Some(tok) if tok.kind == kind)
    }

    // fn expect<F>(&mut self, pred: F) -> Result<token::Token>
    // where
    //     F: Fn(token::Token) -> bool,
    // {
    //     let expect = kind.to_string();
    //     self.bump_if(|tok| tok.kind == kind)
    //         .ok_or_else(|| self.expected_err(expect.as_str()))
    // }

    // fn expect_kind(&mut self, kind: token::TokenKind) -> Result<token::Token> {
    //     let expect = kind.to_string();
    //     self.bump_if(|tok| tok.kind == kind)
    //         .ok_or_else(|| self.expected_err(expect.as_str()))
    // }

    // fn new_ident(&self, ident: token::Token) -> &'src AstNode<'src> {
    //     self.arena.alloc(AstNode::new(
    //         self.gen_id(),
    //         AstKind::new_ident(ident.raw, ident.pos),
    //     ))
    // }

    // fn parse_ident(&mut self) -> Result<&'src AstNode<'src>> {
    //     let ident = self.expect_kind(token::TokenKind::Ident)?;
    //     Ok(self.new_ident(ident))
    // }

    // pub fn parse_fun(&mut self) -> Result<&'src AstNode<'src>> {
    //     self.expect_kind(token::TokenKind::KwFun)?;
    //     self.skip_spaces();
    //     let name = self.parse_ident()?;
    //     self.skip_spaces();
    //     let args = self.parse_fun_args()?;
    //     self.skip_spaces();
    //     let ret_ty = match self.peek() {
    //         Some(tok) if tok.kind == token::TokenKind::LBrace => None,
    //         _ => {
    //             // has return type
    //             self.expect_kind(token::TokenKind::Arrow)?;
    //             self.skip_spaces();
    //             let ret_ty = self.parse_ident()?;
    //             self.skip_spaces();
    //             Some(ret_ty)
    //         }
    //     };
    //     let block = self.parse_block()?;
    //     Ok(self.arena.alloc(AstNode::new(
    //         self.gen_id(),
    //         AstKind::new_fun(name, args, ret_ty, block),
    //     )))
    // }

    // fn parse_block(&mut self) -> Result<&'src AstNode<'src>> {
    //     let lbrace = self.expect_kind(token::TokenKind::LBrace)?;
    //     let pos = lbrace.pos;
    //     let mut stmts = Vec::new();
    //     loop {
    //         self.skip_spaces();
    //         match self.peek() {
    //             None => break,
    //             Some(tok) => match tok.kind {
    //                 token::TokenKind::Semi => {
    //                     // empty stmt
    //                     self.bump();
    //                     continue;
    //                 }
    //                 token::TokenKind::RBrace => break,
    //                 _ => {}
    //             },
    //         }
    //         stmts.push(self.parse_stmt()?);
    //     }

    //     self.expect_kind(token::TokenKind::RBrace)?;
    //     Ok(self
    //         .arena
    //         .alloc(AstNode::new(self.gen_id(), AstKind::new_block(stmts, pos))))
    // }

    // /// fun_args    ::= '(' <args>* ')'
    // /// args        ::= <ident> ':' <ident> (',' <args>)?
    // fn parse_fun_args(&mut self) -> Result<FunArgs<'src>> {
    //     self.expect_kind(token::TokenKind::LParen)?;
    //     self.skip_spaces();

    //     let mut args = Vec::new();
    //     if self.is_next_kind(token::TokenKind::RParen) {
    //         // empty args
    //         self.bump();
    //         return Ok(args);
    //     }

    //     loop {
    //         let arg_name = self.parse_ident()?;
    //         self.skip_spaces();
    //         self.expect_kind(token::TokenKind::Colon)?;
    //         self.skip_spaces();
    //         let arg_ty = self.parse_ident()?;
    //         self.skip_spaces();
    //         // args.push(FunArg::new(self.gen_id(), arg_name, arg_ty));
    //         args.push(FunArg::new(arg_name, arg_ty));
    //         if self.is_next_kind(token::TokenKind::Comma) {
    //             // has next arg
    //             self.bump();
    //             self.skip_spaces();
    //         } else {
    //             break;
    //         }
    //     }

    //     self.expect_kind(token::TokenKind::RParen)?;
    //     Ok(args)
    // }

    fn parse_var_decl(&mut self) -> Result<Stmt> {
        self.bump_if(|tok| matches!(tok.try_as_keyword(), Some(token::Keyword::Var)))
            .ok_or_else(|| self.expected_err("var"))?;
        self.skip_spaces();
        let name = self
            .bump_if(|tok| tok.is_ident() && !tok.is_keyword())
            .ok_or_else(|| self.compile_error(ParseError::InvalidVarDecl))?;
        self.skip_spaces();
        self.bump_if(|tok| tok.is_kind(token::TokenKind::Eq))
            .ok_or_else(|| self.expected_err("="))?;
        self.skip_spaces();
        let expr = self.parse_expr()?;
        Ok(Stmt::new(self.gen_id(), StmtKind::VarDecl(name, expr)))
    }

    fn parse_ret(&mut self) -> Result<Stmt> {
        self.bump_if(|tok| matches!(tok.try_as_keyword(), Some(token::Keyword::Ret)))
            .ok_or_else(|| self.expected_err("ret"))?;
        self.skip_spaces();
        let tok = self
            .peek()
            .ok_or_else(|| self.compile_error(ParseError::UnexpectedEof))?;
        let expr = match tok.kind {
            token::TokenKind::Semi => None,
            _ => Some(self.parse_expr()?),
        };
        Ok(Stmt::new(self.gen_id(), StmtKind::Ret(expr)))
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt> {
        self.skip_spaces();
        let tok = self
            .peek()
            .ok_or_else(|| self.compile_error(ParseError::UnexpectedEof))?;
        let stmt = match tok.kind {
            token::TokenKind::Semi => Stmt::new_empty(self.gen_id()),
            token::TokenKind::Ident if tok.is_keyword() => match tok.try_as_keyword().unwrap() {
                token::Keyword::Ret => self.parse_ret()?,
                token::Keyword::Var => self.parse_var_decl()?,
                token::Keyword::Fun => {
                    return Err(CompileError::new(
                        self.src.pos_from_offset(tok.offset),
                        ParseError::InvalidStmt.into(),
                    ))
                }
            },
            _ => Stmt::new(self.gen_id(), StmtKind::Expr(self.parse_expr()?)),
        };
        self.bump_if(|tok| tok.kind == token::TokenKind::Semi)
            .ok_or_else(|| self.expected_err(";"))?;
        Ok(stmt)
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_assoc_expr_with(0)
    }

    fn parse_assoc_expr_with(&mut self, prec: u8) -> Result<Expr> {
        let mut lhs = self.parse_prefix_expr()?;
        self.skip_spaces();
        let mut bin_op = match self.peek() {
            Some(tok) if tok.kind.is_bin_op() => {
                let bin_op = BinOp::from(tok.kind);
                if bin_op.precedence() < prec
                    || bin_op.precedence() == prec && bin_op.assoc().is_left()
                {
                    return Ok(lhs);
                }
                self.bump();
                bin_op
            }
            _ => return Ok(lhs),
        };
        self.skip_spaces();
        let mut rhs = self.parse_assoc_expr_with(bin_op.precedence())?;
        loop {
            lhs = Expr::new_binary(self.gen_id(), bin_op, Box::new(lhs), Box::new(rhs));
            bin_op = match self.peek() {
                Some(tok) if tok.kind.is_bin_op() => {
                    let bin_op = BinOp::from(tok.kind);
                    if bin_op.precedence() < prec
                        || bin_op.precedence() == prec && bin_op.assoc().is_left()
                    {
                        return Ok(lhs);
                    }
                    self.bump();
                    bin_op
                }
                _ => return Ok(lhs),
            };
            self.skip_spaces();
            rhs = self.parse_assoc_expr_with(bin_op.precedence())?;
        }
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr> {
        let tok = self
            .peek()
            .ok_or_else(|| self.compile_error(ParseError::UnexpectedEof))?;
        match tok.kind {
            token::TokenKind::Minus | token::TokenKind::Not => {
                let unary_op = self.bump().unwrap().kind.into();
                self.skip_spaces();
                let expr = self.parse_prefix_expr()?;
                Ok(Expr::new_unary(self.gen_id(), unary_op, Box::new(expr)))
            }
            _ => self.parse_primary_expr(),
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expr> {
        self.bump_if(|tok| {
            matches!(
                tok.kind,
                token::TokenKind::Ident | token::TokenKind::Lit(_) | token::TokenKind::LParen
            )
        })
        .ok_or_else(|| self.compile_error(ParseError::InvalidExpr))
        .and_then(|tok| match tok.kind {
            token::TokenKind::Lit(kind) => {
                Ok(Expr::new_lit(self.gen_id(), LitKind::from(kind), tok))
            }
            token::TokenKind::Ident => {
                if tok.is_keyword() {
                    Err(CompileError::new(
                        self.src.pos_from_offset(tok.offset),
                        ParseError::CannotUseKeyword(tok.raw).into(),
                    ))
                } else {
                    Ok(Expr::new_var(self.gen_id(), tok))
                }
            }
            token::TokenKind::LParen => {
                self.skip_spaces();
                let expr = self.parse_expr()?;
                self.skip_spaces();
                if !self.is_next_kind(token::TokenKind::RParen) {
                    return Err(self.compile_error(ParseError::UnclosedParenExpr));
                }
                self.bump();
                Ok(expr)
            }
            _ => unreachable!(),
        })
    }
}
