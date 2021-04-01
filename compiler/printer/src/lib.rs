use parse::ast;
use ptree::TreeBuilder;
use typeck::mir;
use typeck::mir::Typed;

#[cfg(test)]
mod tests;

pub fn print(stmts: &[mir::Stmt]) -> std::io::Result<()> {
    let mut printer = Printer::new();
    for stmt in stmts.iter() {
        printer.build_stmt(stmt);
    }
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

    fn build_stmt(&mut self, stmt: &mir::Stmt) -> &mut Self {
        match stmt {
            mir::Stmt::VarDecl(var_decl) => {
                // var_decl.
                self.begin_child("VarDecl")
                    .add_empty_child(&var_decl.name.raw)
                    .add_empty_child("=")
                    .build_expr(&var_decl.init)
                    .end_child()
            }
            mir::Stmt::Expr(expr) => self.build_expr(expr),
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
            mir::Expr::Ident(ident) => self.add_empty_child(&format!("{}::{:?}", ident.raw, ty)),
            mir::Expr::Binary(binary) => self
                .begin_child(&format!("Binary::{:?}", ty))
                .build_expr(&binary.lhs)
                .add_empty_child(&binary.op.symbol)
                .build_expr(&binary.rhs)
                .end_child(),
            mir::Expr::Unary(unary) => unimplemented!(),
        }
    }

    fn print_tree(&mut self) -> std::io::Result<()> {
        let tree = self.builder.build();
        ptree::print_tree(&tree)
    }
}
