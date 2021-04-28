use crate::ast::*;
use span::Offset;

pub enum Node<'a> {
    Fun(&'a Fun),
    FunArg(&'a FunArg),
    Ty(&'a Ty),
    Block(&'a Block),
    Stmt(&'a Stmt),
    Expr(&'a Expr),
}

impl<'a> Node<'a> {
    pub fn as_stmt(&self) -> &'a Stmt {
        match self {
            Self::Stmt(stmt) => stmt,
            _ => panic!(),
        }
    }

    pub fn offset(&self) -> Offset {
        match self {
            Self::Fun(fun) => fun.name.offset,
            Self::FunArg(arg) => arg.name.offset,
            Self::Ty(ty) => ty.offset(),
            Self::Block(block) => block.offset(),
            Self::Stmt(stmt) => stmt.offset(),
            Self::Expr(expr) => expr.offset(),
        }
    }
}

pub fn visit<'a, T, F: Fn(Node<'a>) -> T>(fun: &'a Fun, id: NodeId, callback: F) -> Option<T> {
    if fun.id == id {
        return Some(callback(Node::Fun(fun)));
    }
    for arg in fun.args.iter() {
        if arg.id == id {
            return Some(callback(Node::FunArg(arg)));
        }
        if arg.ty.id == id {
            return Some(callback(Node::Ty(&arg.ty)));
        }
    }
    if let Some(ret_ty) = &fun.ret_ty {
        if ret_ty.id == id {
            return Some(callback(Node::Ty(&ret_ty)));
        }
    }
    if fun.block.id == id {
        return Some(callback(Node::Block(&fun.block)));
    }
    for stmt in fun.block.stmts.iter() {
        if stmt.id == id {
            return Some(callback(Node::Stmt(stmt)));
        }
        match &stmt.kind {
            StmtKind::VarDecl {
                keyword: _,
                name: _,
                init,
            } => {
                if let Some(t) = visit_expr(init, id, &callback) {
                    return Some(t);
                }
            }
            StmtKind::Expr(expr) => {
                if let Some(t) = visit_expr(expr, id, &callback) {
                    return Some(t);
                }
            }
            _ => {}
        };
    }
    None
}

fn visit_expr<'a, T, F: Fn(Node<'a>) -> T>(expr: &'a Expr, id: NodeId, callback: &F) -> Option<T> {
    if expr.id == id {
        return Some(callback(Node::Expr(expr)));
    }
    match &expr.kind {
        ExprKind::Binary(_, lhs, rhs) => {
            if let Some(t) = visit_expr(&lhs, id, callback) {
                Some(t)
            } else {
                visit_expr(&rhs, id, callback)
            }
        }
        ExprKind::Unary(_, expr) => visit_expr(&expr, id, callback),
        _ => None,
    }
}
