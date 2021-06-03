use crate::*;

macro_rules! enum_node {
    ($($name:ident),*) => {
        /// ASTs which has `id: NodeId` field
        pub enum Node<'a> {
            $(
                $name(&'a $name),
            )*
        }

        impl<'a> Node<'a> {
            pub fn id(&self) -> NodeId {
                match self {
                $(
                    Self::$name(inner) => inner.id,
                )*
                }
            }
        }
    }
}

enum_node!(
    Fun, Ident, KwIdent, FunArg, RetTy, Ty, Block, EmptyStmt, RetStmt, VarDecl, Assign, IfStmt,
    Path, Call, Lit, Binary, BinOp, Unary, UnOp
);

pub fn find_node<F>(module: &Module, id: NodeId, callback: F)
where
    F: Fn(Node<'_>),
{
    visit_module(module, |node| {
        if node.id() == id {
            callback(node);
        }
    });
}

pub fn visit_module<F>(module: &Module, f: F)
where
    F: Fn(Node<'_>),
{
    for fun in &module.funs {
        visit_fun(&fun, &f);
    }
}

pub fn visit_fun<F>(fun: &Fun, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::Fun(fun));
    f(Node::KwIdent(&fun.keyword));
    f(Node::Ident(&fun.name));
    visit_fun_args(&fun.args, f);
    if let Some(ret_ty) = &fun.ret_ty {
        visit_ret_ty(&ret_ty, f);
    }
    visit_block(&fun.block, f);
}

fn visit_fun_args<F>(args: &[FunArg], f: &F)
where
    F: Fn(Node<'_>),
{
    for arg in args.iter() {
        f(Node::FunArg(arg));
        if let Some(keyword) = &arg.keyword {
            f(Node::KwIdent(&keyword));
        }
        f(Node::Ident(&arg.name));
        visit_ty(&arg.ty, f);
    }
}

fn visit_ty<F>(ty: &Ty, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::Ty(ty));
    match &ty.kind {
        TyKind::Basic(ident) => f(Node::Ident(ident)),
        TyKind::Ptr(ty) => visit_ty(&ty, f),
    }
}

fn visit_ret_ty<F>(ret_ty: &RetTy, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::RetTy(ret_ty));
    if let Some(keyword) = &ret_ty.keyword {
        f(Node::KwIdent(&keyword));
    }
    visit_ty(&ret_ty.ty, f);
}

fn visit_block<F>(block: &Block, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::Block(block));
    for stmt in block.stmts.iter() {
        visit_stmt(&stmt, f);
    }
}

fn visit_stmt<F>(stmt: &Stmt, f: &F)
where
    F: Fn(Node<'_>),
{
    match &stmt {
        Stmt::Empty(inner) => visit_empty_stmt(inner, f),
        Stmt::Expr(inner) => visit_expr(inner, f),
        Stmt::Ret(inner) => visit_ret_stmt(inner, f),
        Stmt::VarDecl(inner) => visit_var_decl(inner, f),
        Stmt::Assign(inner) => visit_assign(inner, f),
        Stmt::If(inner) => visit_if_stmt(inner, f),
    }
}

fn visit_empty_stmt<F>(stmt: &EmptyStmt, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::EmptyStmt(stmt));
}

fn visit_ret_stmt<F>(stmt: &RetStmt, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::RetStmt(stmt));
    f(Node::KwIdent(&stmt.keyword));
    if let Some(expr) = &stmt.expr {
        visit_expr(&expr, f);
    }
}

fn visit_var_decl<F>(var_decl: &VarDecl, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::VarDecl(var_decl));
    f(Node::KwIdent(&var_decl.keyword));
    f(Node::Ident(&var_decl.name));
    if let Some(ty) = &var_decl.ty {
        visit_ty(&ty, f);
    }
    visit_expr(&var_decl.init, f);
}

fn visit_assign<F>(stmt: &Assign, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::Assign(stmt));
    visit_expr(&stmt.left, f);
    visit_expr(&stmt.right, f);
}

fn visit_if_stmt<F>(stmt: &IfStmt, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::IfStmt(stmt));
    f(Node::KwIdent(&stmt.keyword));
    if let Some(expr) = &stmt.expr {
        visit_expr(&expr, f);
    }
    visit_block(&stmt.block, f);
    if let Some(if_stmt) = &stmt.else_if {
        visit_if_stmt(&if_stmt, f);
    }
}

fn visit_expr<F>(expr: &Expr, f: &F)
where
    F: Fn(Node<'_>),
{
    match &expr {
        Expr::Path(inner) => visit_path(inner, f),
        Expr::Call(inner) => visit_call(inner, f),
        Expr::Lit(inner) => visit_lit(inner, f),
        Expr::Binary(inner) => visit_binary(inner, f),
        Expr::Unary(inner) => visit_unary(inner, f),
    }
}

fn visit_path<F>(path: &Path, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::Path(path));
    f(Node::Ident(&path.ident));
}

fn visit_call<F>(call: &Call, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::Call(call));
    f(Node::Path(&call.path));
    for arg in call.args.iter() {
        visit_expr(&arg, f);
    }
}

fn visit_lit<F>(lit: &Lit, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::Lit(lit));
}

fn visit_binary<F>(binary: &Binary, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::Binary(binary));
    f(Node::BinOp(&binary.op));
    visit_expr(&binary.lhs, f);
    visit_expr(&binary.rhs, f);
}

fn visit_unary<F>(unary: &Unary, f: &F)
where
    F: Fn(Node<'_>),
{
    f(Node::Unary(unary));
    f(Node::UnOp(&unary.op));
}
