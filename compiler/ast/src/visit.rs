use crate::*;

pub fn find_node(module: &Module, id: NodeId) -> Option<&dyn Node> {
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

fn visit_module<'a, F>(module: &'a Module, f: F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    for fun in &module.funs {
        return_some!(visit_fun(&fun, &f))
    }
    for strukt in &module.structs {
        return_some!(visit_struct(&strukt, &f))
    }
    None
}

fn visit_struct<'a, F>(strukt: &'a Struct, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(strukt));
    return_some!(f(&strukt.keyword));
    return_some!(f(&strukt.name));
    return_some!(visit_fields(&strukt.fields, f));
    None
}

fn visit_fields<'a, F>(fields: &'a [Field], f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    for field in fields.iter() {
        return_some!(f(field));
        if let Some(keyword) = &field.keyword {
            return_some!(f(keyword));
        }
        return_some!(f(&field.name));
        return_some!(visit_ty(&field.ty, f));
    }
    None
}

fn visit_fun<'a, F>(fun: &'a Fun, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(fun));
    return_some!(f(&fun.keyword));
    return_some!(f(&fun.name));
    return_some!(visit_fun_args(&fun.args, f));
    if let Some(ret_ty) = &fun.ret_ty {
        return_some!(visit_ret_ty(&ret_ty, f));
    }
    return_some!(visit_block(&fun.block, f));
    None
}

fn visit_fun_args<'a, F>(args: &'a [FunArg], f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    for arg in args.iter() {
        return_some!(f(arg));
        if let Some(keyword) = &arg.keyword {
            return_some!(f(keyword));
        }
        return_some!(f(&arg.name));
        return_some!(visit_ty(&arg.ty, f));
    }
    None
}

fn visit_ty<'a, F>(ty: &'a Ty, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(ty));
    match &ty.kind {
        TyKind::Raw(ident) => f(ident),
        TyKind::Ptr(ty) => visit_ty(&ty, f),
    }
}

fn visit_ret_ty<'a, F>(ret_ty: &'a RetTy, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(ret_ty));
    if let Some(keyword) = &ret_ty.keyword {
        return_some!(f(keyword));
    }
    return_some!(visit_ty(&ret_ty.ty, f));
    None
}

fn visit_block<'a, F>(block: &'a Block, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(block));
    for stmt in block.stmts.iter() {
        return_some!(visit_stmt(&stmt, f));
    }
    None
}

fn visit_stmt<'a, F>(stmt: &'a Stmt, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
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

fn visit_empty_stmt<'a, F>(stmt: &'a EmptyStmt, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(stmt));
    None
}

fn visit_ret_stmt<'a, F>(stmt: &'a RetStmt, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(stmt));
    return_some!(f(&stmt.keyword));
    if let Some(expr) = &stmt.expr {
        return_some!(visit_expr(&expr, f));
    }
    None
}

fn visit_var_decl<'a, F>(var_decl: &'a VarDecl, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(var_decl));
    return_some!(f(&var_decl.keyword));
    return_some!(f(&var_decl.name));
    if let Some(ty) = &var_decl.ty {
        return_some!(visit_ty(&ty, f));
    }
    return_some!(visit_expr(&var_decl.init, f));
    None
}

fn visit_assign<'a, F>(stmt: &'a Assign, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(stmt));
    return_some!(visit_expr(&stmt.left, f));
    return_some!(visit_expr(&stmt.right, f));
    None
}

fn visit_if_stmt<'a, F>(stmt: &'a IfStmt, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(stmt));
    return_some!(f(&stmt.keyword));
    if let Some(expr) = &stmt.expr {
        return_some!(visit_expr(&expr, f));
    }
    return_some!(visit_block(&stmt.block, f));
    if let Some(if_stmt) = &stmt.else_if {
        return_some!(visit_if_stmt(&if_stmt, f));
    }
    None
}

fn visit_expr<'a, F>(expr: &'a Expr, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    match &expr {
        Expr::Path(inner) => visit_path(inner, f),
        Expr::Call(inner) => visit_call(inner, f),
        Expr::Lit(inner) => visit_lit(inner, f),
        Expr::Binary(inner) => visit_binary(inner, f),
        Expr::Unary(inner) => visit_unary(inner, f),
    }
}

fn visit_path<'a, F>(path: &'a Path, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(path));
    return_some!(f(&path.ident));
    None
}

fn visit_call<'a, F>(call: &'a Call, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(call));
    return_some!(f(&call.path));
    for arg in call.args.iter() {
        return_some!(visit_expr(&arg, f));
    }
    None
}

fn visit_lit<'a, F>(lit: &'a Lit, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(lit));
    None
}

fn visit_binary<'a, F>(binary: &'a Binary, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(binary));
    return_some!(f(&binary.op));
    return_some!(visit_expr(&binary.lhs, f));
    return_some!(visit_expr(&binary.rhs, f));
    None
}

fn visit_unary<'a, F>(unary: &'a Unary, f: &F) -> Option<&'a dyn Node>
where
    F: Fn(&'a dyn Node) -> Option<&'a dyn Node>,
{
    return_some!(f(unary));
    return_some!(f(&unary.op));
    None
}
