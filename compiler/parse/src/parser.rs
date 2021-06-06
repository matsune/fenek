use ast::*;
use error::{CompileError, ParseError, Result};
use lex::token;
use pos::Pos;
use std::collections::VecDeque;
use std::convert::TryFrom;

fn is_ident(tok: &token::Token) -> bool {
    tok.kind == token::TokenKind::Ident
}

pub struct Parser {
    tokens: VecDeque<token::Token>,
    node_id: NodeId,
    last_pos: Pos,
}

impl Parser {
    pub fn new(tokens: VecDeque<token::Token>) -> Self {
        Parser {
            tokens,
            node_id: 0,
            last_pos: Pos::default(),
        }
    }

    fn peek(&self) -> Option<&token::Token> {
        self.tokens.front()
    }

    fn bump(&mut self) -> Option<token::Token> {
        let tok = self.tokens.pop_front();
        if let Some(tok) = &tok {
            self.last_pos = tok.pos;
        }
        tok
    }

    fn gen_id(&mut self) -> NodeId {
        self.node_id += 1;
        self.node_id
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
                            | token::TokenKind::BlockComment
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
            self.last_pos,
            Box::new(ParseError::Expected(expected.to_string())),
        )
    }

    fn compile_error(&self, error: ParseError) -> CompileError {
        CompileError::new(self.last_pos, error.into())
    }

    fn is_next_kind(&self, kind: token::TokenKind) -> bool {
        matches!(self.peek(), Some(tok) if tok.kind == kind)
    }

    fn bump_if_kind(&mut self, kind: token::TokenKind) -> Option<token::Token> {
        self.bump_if(|tok| tok.kind == kind)
    }

    fn bump_kind(&mut self, kind: token::TokenKind) -> Result<token::Token> {
        self.bump_if(|tok| tok.kind == kind)
            .ok_or_else(|| self.expected_err(kind))
    }

    fn bump_keyword(&mut self, keyword: token::Keyword) -> Result<ast::KwIdent> {
        let (kind, pos) = self
            .bump_if(|tok| matches!(tok.try_as_keyword(), Some(kw) if kw == keyword))
            .map(|tok| (tok.try_as_keyword().unwrap(), tok.pos))
            .ok_or_else(|| self.expected_err(keyword))?;
        Ok(ast::KwIdent {
            id: self.gen_id(),
            kind,
            pos,
        })
    }

    pub fn parse_module(&mut self) -> Result<Module> {
        let id = self.gen_id();
        let mut funs = Vec::new();
        self.skip_spaces();
        while self.peek().is_some() {
            funs.push(self.parse_fun()?);
            self.skip_spaces();
        }
        Ok(Module { id, funs })
    }

    fn parse_ident(&mut self) -> Result<ast::Ident> {
        let ident = self
            .bump_if(is_ident)
            .ok_or_else(|| self.expected_err("ident"))?;
        if let Some(keyword) = ident.try_as_keyword() {
            Err(self.compile_error(ParseError::FoundKeyword(keyword.to_string())))
        } else {
            Ok(ast::Ident {
                id: self.gen_id(),
                raw: ident.raw,
                pos: ident.pos,
            })
        }
    }

    fn bump_if_mut_or_let(&mut self) -> Option<ast::KwIdent> {
        self.bump_if(|tok| {
            matches!(
                tok.try_as_keyword(),
                Some(token::Keyword::Mut) | Some(token::Keyword::Let)
            )
        })
        .map(|tok| {
            let pos = tok.pos;
            let kind = tok.try_as_keyword().unwrap();
            KwIdent {
                id: self.gen_id(),
                kind,
                pos,
            }
        })
    }

    fn parse_fun(&mut self) -> Result<Fun> {
        let id = self.gen_id();
        let keyword = self.bump_keyword(token::Keyword::Fun)?;
        self.skip_spaces();

        let name = self.parse_ident()?;
        self.skip_spaces();

        let args = self.parse_fun_args()?;
        self.skip_spaces();

        let ret_ty = if self.is_next_kind(token::TokenKind::LBrace) {
            None
        } else {
            // has return type
            self.bump_if_kind(token::TokenKind::Arrow)
                .ok_or_else(|| self.expected_err("->"))?;
            self.skip_spaces();
            Some(self.parse_ret_ty()?)
        };
        self.skip_spaces();

        let block = self.parse_block()?;
        Ok(Fun {
            id,
            keyword,
            name,
            args,
            ret_ty,
            block,
        })
    }

    fn parse_fun_args(&mut self) -> Result<FunArgs> {
        self.bump_kind(token::TokenKind::LParen)?;
        self.skip_spaces();

        let mut args = Vec::new();
        if self.bump_if_kind(token::TokenKind::RParen).is_some() {
            return Ok(args);
        }

        loop {
            let keyword = self.bump_if_mut_or_let();
            self.skip_spaces();

            let name = self.parse_ident()?;
            self.skip_spaces();

            self.bump_kind(token::TokenKind::Colon)?;
            self.skip_spaces();

            let ty = self.parse_ty()?;
            args.push(FunArg {
                id: self.gen_id(),
                keyword,
                name,
                ty,
            });
            self.skip_spaces();

            if self.bump_if_kind(token::TokenKind::Comma).is_some() {
                // has next arg
                self.skip_spaces();
            } else {
                break;
            }
        }
        self.bump_kind(token::TokenKind::RParen)?;
        Ok(args)
    }

    fn parse_ret_ty(&mut self) -> Result<RetTy> {
        let id = self.gen_id();
        let keyword = self.bump_if_mut_or_let();
        self.skip_spaces();
        let ty = self.parse_ty()?;
        Ok(RetTy { id, keyword, ty })
    }

    fn parse_ty(&mut self) -> Result<Ty> {
        let ident = self.parse_ident()?;
        let mut ty = Ty::new_basic(self.gen_id(), ident);
        loop {
            self.skip_spaces();
            if self
                .bump_if(|tok| tok.kind == token::TokenKind::Star)
                .is_some()
            {
                ty = Ty::new_ptr(self.gen_id(), ty);
            } else {
                return Ok(ty);
            }
        }
    }

    fn parse_block(&mut self) -> Result<Block> {
        let id = self.gen_id();
        let pos = self.bump_kind(token::TokenKind::LBrace)?.pos;
        let mut stmts = Vec::new();
        loop {
            self.skip_spaces();
            match self.peek() {
                None => break,
                Some(tok) => match tok.kind {
                    token::TokenKind::RBrace => break,
                    _ => {
                        stmts.push(self.parse_stmt()?);
                    }
                },
            }
        }
        self.bump_kind(token::TokenKind::RBrace)?;
        Ok(Block { id, stmts, pos })
    }

    fn parse_var_decl(&mut self) -> Result<Stmt> {
        let id = self.gen_id();
        let keyword = self.bump_if_mut_or_let().unwrap();
        self.skip_spaces();

        let name = self.parse_ident()?;
        self.skip_spaces();

        let ty = if self.bump_if_kind(token::TokenKind::Colon).is_some() {
            self.skip_spaces();
            Some(self.parse_ty()?)
        } else {
            None
        };
        self.skip_spaces();

        self.bump_kind(token::TokenKind::Eq)?;
        self.skip_spaces();

        let init = self.parse_expr()?;
        Ok(Stmt::VarDecl(VarDecl {
            id,
            keyword,
            name,
            ty,
            init,
        }))
    }

    fn parse_ret(&mut self) -> Result<Stmt> {
        let id = self.gen_id();
        let keyword = self.bump_keyword(token::Keyword::Ret).unwrap();
        self.skip_spaces();

        let tok = self
            .peek()
            .ok_or_else(|| self.compile_error(ParseError::UnexpectedEof))?;
        let expr = match tok.kind {
            token::TokenKind::Semi => None,
            _ => Some(self.parse_expr()?),
        };
        Ok(Stmt::Ret(RetStmt { id, keyword, expr }))
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt> {
        let tok = self
            .peek()
            .ok_or_else(|| self.compile_error(ParseError::UnexpectedEof))?;
        let stmt = match tok.kind {
            token::TokenKind::Semi => {
                let pos = tok.pos;
                Stmt::Empty(EmptyStmt {
                    id: self.gen_id(),
                    pos,
                })
            }
            token::TokenKind::Ident if tok.try_as_keyword().is_some() => {
                match tok.try_as_keyword().unwrap() {
                    token::Keyword::Ret => self.parse_ret()?,
                    token::Keyword::Mut | token::Keyword::Let => self.parse_var_decl()?,
                    token::Keyword::If => return self.parse_if(),
                    token::Keyword::Fun | token::Keyword::Else => {
                        return Err(self.compile_error(ParseError::InvalidStmt))
                    }
                }
            }
            _ => {
                let expr = self.parse_expr()?;
                self.skip_spaces();
                if self.bump_if_kind(token::TokenKind::Eq).is_some() {
                    // assign
                    self.skip_spaces();
                    let right = self.parse_expr()?;
                    self.skip_spaces();
                    Stmt::Assign(Assign {
                        id: self.gen_id(),
                        left: expr,
                        right,
                    })
                } else {
                    Stmt::Expr(expr)
                }
            }
        };
        self.bump_kind(token::TokenKind::Semi)?;
        Ok(stmt)
    }

    fn parse_if(&mut self) -> Result<Stmt> {
        let id = self.gen_id();
        let keyword = self.bump_keyword(token::Keyword::If)?;
        self.skip_spaces();
        let expr = Some(self.parse_expr()?);
        self.skip_spaces();
        let block = self.parse_block()?;
        self.skip_spaces();
        let else_if = match self.peek() {
            Some(tok) if matches!(tok.try_as_keyword(), Some(token::Keyword::Else)) => {
                Some(Box::new(self.parse_else()?))
            }
            _ => None,
        };
        Ok(Stmt::If(IfStmt {
            id,
            keyword,
            expr,
            block,
            else_if,
        }))
    }

    fn parse_else(&mut self) -> Result<IfStmt> {
        let id = self.gen_id();
        let keyword = self.bump_keyword(token::Keyword::Else)?;
        self.skip_spaces();
        let expr = if self.is_next_kind(token::TokenKind::LBrace) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        let block = self.parse_block()?;
        let else_if = match self.peek() {
            Some(tok) if matches!(tok.try_as_keyword(), Some(token::Keyword::Else)) => {
                if expr.is_some() {
                    Some(Box::new(self.parse_else()?))
                } else {
                    None
                }
            }
            _ => None,
        };
        Ok(IfStmt {
            id,
            keyword,
            expr,
            block,
            else_if,
        })
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_assoc_expr_with(0)
    }

    fn parse_assoc_expr_with(&mut self, prec: u8) -> Result<Expr> {
        let mut lhs = self.parse_prefix_expr()?;
        self.skip_spaces();

        let mut bin_op = match self.peek() {
            Some(tok) if BinOpKind::try_from(tok.kind).is_ok() => {
                let op_kind = BinOpKind::try_from(tok.kind).unwrap();
                if op_kind.precedence() < prec
                    || op_kind.precedence() == prec && op_kind.assoc().is_left()
                {
                    // If the parsed BinOpKind is lower priority than
                    // previous operator like this:
                    //
                    //  prec
                    //   ↓
                    // 4 * 3 - 2
                    //       ↑
                    //     op_kind
                    //
                    // `4 * 3` will be a lhs and `2` will be a rhs at here.
                    // Priority will be determined by an associativity of that
                    // operator if the precedences are same.
                    //
                    // 4 + 3 + 2
                    // → (4 + 3) + 2
                    // (because `+` is left associativity)
                    return Ok(lhs);
                }
                self.bump();
                BinOp {
                    id: self.gen_id(),
                    kind: op_kind,
                    pos: Pos::default(),
                }
            }
            _ => return Ok(lhs),
        };
        self.skip_spaces();

        let mut rhs = self.parse_assoc_expr_with(bin_op.kind.precedence())?;
        loop {
            lhs = Expr::Binary(Binary {
                id: self.gen_id(),
                op: bin_op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
            bin_op = match self.peek() {
                Some(tok) if BinOpKind::try_from(tok.kind).is_ok() => {
                    let op_kind = BinOpKind::try_from(tok.kind).unwrap();
                    if op_kind.precedence() < prec
                        || op_kind.precedence() == prec && op_kind.assoc().is_left()
                    {
                        return Ok(lhs);
                    }
                    self.bump();
                    BinOp {
                        id: self.gen_id(),
                        kind: op_kind,
                        pos: Pos::default(),
                    }
                }
                _ => return Ok(lhs),
            };
            self.skip_spaces();
            rhs = self.parse_assoc_expr_with(bin_op.kind.precedence())?;
        }
    }

    fn parse_prefix_expr(&mut self) -> Result<Expr> {
        let tok = self
            .peek()
            .ok_or_else(|| self.compile_error(ParseError::UnexpectedEof))?;
        if let Ok(op_kind) = ast::UnOpKind::try_from(tok.kind) {
            let pos = self.bump().unwrap().pos;
            self.skip_spaces();
            let op = UnOp {
                id: self.gen_id(),
                kind: op_kind,
                pos,
            };
            let expr = self.parse_prefix_expr()?;
            Ok(Expr::Unary(Unary {
                id: self.gen_id(),
                op,
                expr: Box::new(expr),
            }))
        } else {
            self.parse_postfix_expr()
        }
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr> {
        let primary = self.parse_primary_expr()?;
        self.skip_spaces();
        match primary {
            Expr::Path(path) if self.is_next_kind(token::TokenKind::LParen) => {
                // Call
                self.bump();
                self.skip_spaces();

                let mut args = Vec::new();
                if self.bump_if_kind(token::TokenKind::RParen).is_some() {
                    return Ok(Expr::Call(Call {
                        id: self.gen_id(),
                        path,
                        args,
                    }));
                }

                loop {
                    args.push(self.parse_expr()?);
                    self.skip_spaces();

                    match self.peek() {
                        Some(tok) if tok.kind == token::TokenKind::RParen => {
                            self.bump();
                            break;
                        }
                        Some(tok) if tok.kind == token::TokenKind::Comma => {
                            self.bump();
                            self.skip_spaces();
                        }
                        Some(_) => {
                            return Err(self.expected_err(") or ,".to_string()));
                        }
                        None => return Err(self.compile_error(ParseError::UnexpectedEof)),
                    };
                }
                Ok(Expr::Call(Call {
                    id: self.gen_id(),
                    path,
                    args,
                }))
            }
            _ => Ok(primary),
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expr> {
        let peek = self
            .peek()
            .ok_or_else(|| self.compile_error(ParseError::UnexpectedEof))?;
        match peek.kind {
            token::TokenKind::Lit(kind) => {
                let tok = self.bump().unwrap();
                let pos = tok.pos;
                Ok(Expr::Lit(Lit {
                    id: self.gen_id(),
                    kind,
                    raw: tok.raw,
                    pos,
                }))
            }
            token::TokenKind::Ident => {
                if let Some(keyword) = peek.try_as_keyword() {
                    Err(self.compile_error(ParseError::FoundKeyword(keyword.to_string())))
                } else {
                    let ident = self.parse_ident()?;
                    Ok(Expr::Path(Path {
                        id: self.gen_id(),
                        ident,
                    }))
                }
            }
            token::TokenKind::LParen => {
                self.bump();
                self.skip_spaces();
                let expr = self.parse_expr()?;
                self.skip_spaces();
                if !self.is_next_kind(token::TokenKind::RParen) {
                    return Err(self.compile_error(ParseError::UnclosedParenExpr));
                }
                self.bump();
                Ok(expr)
            }
            _ => Err(self.compile_error(ParseError::InvalidExpr)),
        }
    }
}
