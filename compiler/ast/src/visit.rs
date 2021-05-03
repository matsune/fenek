use crate::*;
use pos::Offset;

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

macro_rules! return_if_some {
    ($e:expr) => {
        let res = $e;
        if res.is_some() {
            return res;
        }
    };
}

pub fn visit_module<'a, T, F: Fn(Node<'a>) -> T>(
    module: &'a Module,
    id: NodeId,
    callback: F,
) -> Option<T> {
    for fun in &module.funs {
        return_if_some!(visit_fun(&fun, id, &callback));
    }
    None
}

pub fn visit_fun<'a, T, F: Fn(Node<'a>) -> T>(fun: &'a Fun, id: NodeId, callback: F) -> Option<T> {
    if fun.id == id {
        return Some(callback(Node::Fun(fun)));
    }

    return_if_some!(visit_fun_args(&fun.args, id, &callback));

    if let Some(ret_ty) = &fun.ret_ty {
        return_if_some!(visit_ty(&ret_ty, id, &callback));
    }

    visit_block(&fun.block, id, &callback)
}

fn visit_fun_args<'a, T, F: Fn(Node<'a>) -> T>(
    args: &'a [FunArg],
    id: NodeId,
    callback: &F,
) -> Option<T> {
    for arg in args.iter() {
        if arg.id == id {
            return Some(callback(Node::FunArg(arg)));
        }
        return_if_some!(visit_ty(&arg.ty, id, callback));
    }
    None
}

fn visit_ty<'a, T, F: Fn(Node<'a>) -> T>(ty: &'a Ty, id: NodeId, callback: &F) -> Option<T> {
    if ty.id == id {
        Some(callback(Node::Ty(&ty)))
    } else {
        None
    }
}

fn visit_block<'a, T, F: Fn(Node<'a>) -> T>(
    block: &'a Block,
    id: NodeId,
    callback: &F,
) -> Option<T> {
    if block.id == id {
        return Some(callback(Node::Block(&block)));
    }
    for stmt in block.stmts.iter() {
        return_if_some!(visit_stmt(stmt, id, &callback));
    }
    None
}

fn visit_stmt<'a, T, F: Fn(Node<'a>) -> T>(stmt: &'a Stmt, id: NodeId, callback: &F) -> Option<T> {
    if stmt.id == id {
        return Some(callback(Node::Stmt(stmt)));
    }
    match &stmt.kind {
        StmtKind::VarDecl {
            keyword: _,
            name: _,
            init,
        } => {
            return_if_some!(visit_expr(init, id, &callback));
        }
        StmtKind::Expr(expr) => {
            return_if_some!(visit_expr(expr, id, &callback));
        }
        StmtKind::Assign(left, right) => {
            return_if_some!(visit_expr(left, id, &callback));
            return_if_some!(visit_expr(right, id, &callback));
        }
        _ => {}
    };
    None
}

fn visit_expr<'a, T, F: Fn(Node<'a>) -> T>(expr: &'a Expr, id: NodeId, callback: &F) -> Option<T> {
    if expr.id == id {
        return Some(callback(Node::Expr(expr)));
    }
    match &expr.kind {
        ExprKind::Binary(_, lhs, rhs) => {
            return_if_some!(visit_expr(&lhs, id, callback));
            visit_expr(&rhs, id, callback)
        }
        ExprKind::Unary(_, expr) => visit_expr(&expr, id, callback),
        _ => None,
    }
}
