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

pub fn visit_module<'a>(module: &'a Module, id: NodeId) -> Option<Node<'a>> {
    for fun in &module.funs {
        return_if_some!(visit_fun(&fun, id));
    }
    None
}

pub fn visit_fun<'a>(fun: &'a Fun, id: NodeId) -> Option<Node<'a>> {
    if fun.id == id {
        return Some(Node::Fun(fun));
    }

    return_if_some!(visit_fun_args(&fun.args, id));

    if let Some(ret_ty) = &fun.ret_ty {
        return_if_some!(visit_ty(&ret_ty, id));
    }

    visit_block(&fun.block, id)
}

fn visit_fun_args<'a>(args: &'a [FunArg], id: NodeId) -> Option<Node<'a>> {
    for arg in args.iter() {
        if arg.id == id {
            return Some(Node::FunArg(arg));
        }
        return_if_some!(visit_ty(&arg.ty, id));
    }
    None
}

fn visit_ty<'a>(ty: &'a Ty, id: NodeId) -> Option<Node> {
    if ty.id == id {
        Some(Node::Ty(&ty))
    } else {
        None
    }
}

fn visit_block<'a>(block: &'a Block, id: NodeId) -> Option<Node<'a>> {
    if block.id == id {
        return Some(Node::Block(&block));
    }
    for stmt in block.stmts.iter() {
        return_if_some!(visit_stmt(stmt, id));
    }
    None
}

fn visit_stmt<'a>(stmt: &'a Stmt, id: NodeId) -> Option<Node<'a>> {
    if stmt.id == id {
        return Some(Node::Stmt(stmt));
    }
    match &stmt.kind {
        StmtKind::VarDecl {
            keyword: _,
            name: _,
            ty: _,
            init,
        } => {
            return_if_some!(visit_expr(init, id));
        }
        StmtKind::Expr(expr) => {
            return_if_some!(visit_expr(expr, id));
        }
        StmtKind::Assign(left, right) => {
            return_if_some!(visit_expr(left, id));
            return_if_some!(visit_expr(right, id));
        }
        StmtKind::Empty(_) => {}
        StmtKind::Ret { keyword, expr } => {
            if let Some(expr) = expr {
                return_if_some!(visit_expr(expr, id));
            }
        }
        StmtKind::If(if_stmt) => {
            return_if_some!(visit_if(&if_stmt, id));
        }
    };
    None
}

fn visit_if<'a>(stmt: &'a IfStmt, id: NodeId) -> Option<Node<'a>> {
    if let Some(expr) = &stmt.expr {
        return_if_some!(visit_expr(&expr, id));
    }
    return_if_some!(visit_block(&stmt.block, id));
    if let Some(else_if) = &stmt.else_if {
        return_if_some!(visit_if(&else_if, id));
    }
    None
}

fn visit_expr<'a>(expr: &'a Expr, id: NodeId) -> Option<Node<'a>> {
    if expr.id == id {
        return Some(Node::Expr(expr));
    }
    match &expr.kind {
        ExprKind::Binary(_, lhs, rhs) => {
            return_if_some!(visit_expr(&lhs, id));
            visit_expr(&rhs, id)
        }
        ExprKind::Unary(_, expr) => visit_expr(&expr, id),
        _ => None,
    }
}
