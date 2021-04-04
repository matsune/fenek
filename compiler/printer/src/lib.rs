use parse::ast;
use ptree::TreeBuilder;
use typeck::mir;
use typeck::mir::Typed;

#[cfg(test)]
mod tests;

pub fn print(fun: &mir::Fun) -> std::io::Result<()> {
    let mut printer = Printer::new();
    printer.build_fun(fun);
    printer.print_tree()
}

struct Printer {
    builder: TreeBuilder,
}

impl Printer {
    fn new() -> Self {
        Self {
            builder: TreeBuilder::new("".to_string()),
        }
    }

    fn begin_child(&mut self, name: &str) -> &mut Self {
        self.builder.begin_child(name.to_string());
        self
    }

    fn end_child(&mut self) -> &mut Self {
        self.builder.end_child();
        self
    }

    fn add_empty_child(&mut self, name: &str) -> &mut Self {
        self.builder.add_empty_child(name.to_string());
        self
    }

    fn build_fun(&mut self, fun: &mir::Fun) -> &mut Self {
        let args = fun
            .args
            .iter()
            .map(|arg| format!("{}: {:?}", arg.raw, arg.def.as_var_def().ty))
            .collect::<Vec<_>>()
            .join(", ");
        let ret_ty = match fun.ret_ty {
            mir::Type::Void => String::from(""),
            _ => format!(" -> {:?}", fun.ret_ty),
        };
        self.begin_child(&format!("fun {}({}){}", fun.name.raw, args, ret_ty));
        for stmt in fun.block.stmts.iter() {
            self.build_stmt(stmt);
        }
        self
    }

    fn build_stmt(&mut self, stmt: &mir::Stmt) -> &mut Self {
        match stmt {
            mir::Stmt::VarDecl(var_decl) => self
                .begin_child("VarDecl")
                .add_empty_child(&format!(
                    "{}::{:?} def_id={}",
                    var_decl.name.raw, var_decl.def.ty, var_decl.def.id,
                ))
                .add_empty_child("=")
                .build_expr(&var_decl.init)
                .end_child(),
            mir::Stmt::Expr(expr) => self.build_expr(expr),
            mir::Stmt::Ret(ret) => {
                self.begin_child("Ret");
                match &ret.expr {
                    Some(expr) => {
                        self.build_expr(&expr);
                    }
                    None => {}
                };
                self.end_child()
            }
        }
    }

    fn build_expr(&mut self, expr: &mir::Expr) -> &mut Self {
        let ty = expr.get_type();
        match expr {
            mir::Expr::Lit(lit) => match &lit.kind {
                ast::LitKind::Int(v) => self.add_empty_child(&format!("{}::{:?}", v, ty)),
                ast::LitKind::Float(v) => self.add_empty_child(&format!("{}::{:?}", v, ty)),
                ast::LitKind::Bool(v) => self.add_empty_child(&format!("{}::{:?}", v, ty)),
                ast::LitKind::String(v) => self.add_empty_child(&format!("{}::{:?}", v, ty)),
            },
            mir::Expr::Ident(ident) => self.add_empty_child(&format!(
                "{}::{:?} def_id={}",
                ident.raw,
                ty,
                ident.def.id()
            )),
            mir::Expr::Binary(binary) => self
                .begin_child(&format!("Binary::{:?}", ty))
                .build_expr(&binary.lhs)
                .add_empty_child(&binary.op.symbol)
                .build_expr(&binary.rhs)
                .end_child(),
            mir::Expr::Unary(unary) => self
                .begin_child(&format!("Unary::{:?}", ty))
                .add_empty_child(&format!("{}", unary.op))
                .build_expr(&unary.expr)
                .end_child(),
        }
    }

    fn print_tree(&mut self) -> std::io::Result<()> {
        let tree = self.builder.build();
        ptree::print_tree(&tree)
    }
}
