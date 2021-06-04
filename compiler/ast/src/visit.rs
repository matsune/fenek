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

            pub fn pos(&self) -> Pos {
                match self {
                $(
                    Self::$name(inner) => inner.pos(),
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

pub fn find_node(module: &Module, id: NodeId) -> Option<Node<'_>> {
    visit_module(
        module,
        |node| {
            if node.id() == id {
                Some(node)
            } else {
                None
            }
        },
    )
}

macro_rules! return_some {
    ($e:expr) => {
        if let Some(n) = $e {
            return Some(n);
        }
    };
}

fn visit_module<'a, F>(module: &'a Module, f: F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    for fun in &module.funs {
        return_some!(visit_fun(&fun, &f))
    }
    None
}

fn visit_fun<'a, F>(fun: &'a Fun, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::Fun(fun)));
    return_some!(f(Node::KwIdent(&fun.keyword)));
    return_some!(f(Node::Ident(&fun.name)));
    return_some!(visit_fun_args(&fun.args, f));
    if let Some(ret_ty) = &fun.ret_ty {
        return_some!(visit_ret_ty(&ret_ty, f));
    }
    return_some!(visit_block(&fun.block, f));
    None
}

fn visit_fun_args<'a, F>(args: &'a [FunArg], f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    for arg in args.iter() {
        return_some!(f(Node::FunArg(arg)));
        if let Some(keyword) = &arg.keyword {
            return_some!(f(Node::KwIdent(&keyword)));
        }
        return_some!(f(Node::Ident(&arg.name)));
        return_some!(visit_ty(&arg.ty, f));
    }
    None
}

fn visit_ty<'a, F>(ty: &'a Ty, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::Ty(ty)));
    match &ty.kind {
        TyKind::Basic(ident) => f(Node::Ident(ident)),
        TyKind::Ptr(ty) => visit_ty(&ty, f),
    }
}

fn visit_ret_ty<'a, F>(ret_ty: &'a RetTy, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::RetTy(ret_ty)));
    if let Some(keyword) = &ret_ty.keyword {
        return_some!(f(Node::KwIdent(&keyword)));
    }
    return_some!(visit_ty(&ret_ty.ty, f));
    None
}

fn visit_block<'a, F>(block: &'a Block, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::Block(block)));
    for stmt in block.stmts.iter() {
        return_some!(visit_stmt(&stmt, f));
    }
    None
}

fn visit_stmt<'a, F>(stmt: &'a Stmt, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
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

fn visit_empty_stmt<'a, F>(stmt: &'a EmptyStmt, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::EmptyStmt(stmt)));
    None
}

fn visit_ret_stmt<'a, F>(stmt: &'a RetStmt, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::RetStmt(stmt)));
    return_some!(f(Node::KwIdent(&stmt.keyword)));
    if let Some(expr) = &stmt.expr {
        return_some!(visit_expr(&expr, f));
    }
    None
}

fn visit_var_decl<'a, F>(var_decl: &'a VarDecl, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::VarDecl(var_decl)));
    return_some!(f(Node::KwIdent(&var_decl.keyword)));
    return_some!(f(Node::Ident(&var_decl.name)));
    if let Some(ty) = &var_decl.ty {
        return_some!(visit_ty(&ty, f));
    }
    return_some!(visit_expr(&var_decl.init, f));
    None
}

fn visit_assign<'a, F>(stmt: &'a Assign, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::Assign(stmt)));
    return_some!(visit_expr(&stmt.left, f));
    return_some!(visit_expr(&stmt.right, f));
    None
}

fn visit_if_stmt<'a, F>(stmt: &'a IfStmt, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::IfStmt(stmt)));
    return_some!(f(Node::KwIdent(&stmt.keyword)));
    if let Some(expr) = &stmt.expr {
        return_some!(visit_expr(&expr, f));
    }
    return_some!(visit_block(&stmt.block, f));
    if let Some(if_stmt) = &stmt.else_if {
        return_some!(visit_if_stmt(&if_stmt, f));
    }
    None
}

fn visit_expr<'a, F>(expr: &'a Expr, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    match &expr {
        Expr::Path(inner) => visit_path(inner, f),
        Expr::Call(inner) => visit_call(inner, f),
        Expr::Lit(inner) => visit_lit(inner, f),
        Expr::Binary(inner) => visit_binary(inner, f),
        Expr::Unary(inner) => visit_unary(inner, f),
    }
}

fn visit_path<'a, F>(path: &'a Path, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::Path(path)));
    return_some!(f(Node::Ident(&path.ident)));
    None
}

fn visit_call<'a, F>(call: &'a Call, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::Call(call)));
    return_some!(f(Node::Path(&call.path)));
    for arg in call.args.iter() {
        return_some!(visit_expr(&arg, f));
    }
    None
}

fn visit_lit<'a, F>(lit: &'a Lit, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::Lit(lit)));
    None
}

fn visit_binary<'a, F>(binary: &'a Binary, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::Binary(binary)));
    return_some!(f(Node::BinOp(&binary.op)));
    return_some!(visit_expr(&binary.lhs, f));
    return_some!(visit_expr(&binary.rhs, f));
    None
}

fn visit_unary<'a, F>(unary: &'a Unary, f: &F) -> Option<Node<'a>>
where
    F: Fn(Node<'a>) -> Option<Node<'a>>,
{
    return_some!(f(Node::Unary(unary)));
    return_some!(f(Node::UnOp(&unary.op)));
    None
}
