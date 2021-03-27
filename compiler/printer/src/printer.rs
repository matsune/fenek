use parse::ast;
use ptree::{TreeBuilder, TreeItem};

#[cfg(test)]
mod tests;

struct Printer {
    builder: TreeBuilder,
}

impl Printer {
    fn new() -> Self {
        Self {
            builder: TreeBuilder::new("ast".to_string()),
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

    fn build_stmt(&mut self, stmt: &ast::Stmt) -> &mut Self {
        match stmt {
            ast::Stmt::VarDecl(var_decl) => self
                .begin_child("VarDecl")
                .add_empty_child(&var_decl.name.raw)
                .add_empty_child("=")
                .build_expr(&var_decl.init)
                .end_child(),
            ast::Stmt::Expr(expr) => unimplemented!(),
        }
    }

    fn build_expr(&mut self, expr: &ast::Expr) -> &mut Self {
        match expr {
            ast::Expr::Lit(lit) => match &lit.kind {
                ast::LitKind::Int(v) => self.add_empty_child(&format!("{}", v)),
                ast::LitKind::Float(v) => self.add_empty_child(&format!("{}", v)),
                ast::LitKind::Bool(v) => self.add_empty_child(&format!("{}", v)),
                ast::LitKind::String(v) => self.add_empty_child(&v),
            },
            ast::Expr::Ident(ident) => self.add_empty_child(&ident.raw),
            ast::Expr::Binary(binary) => self
                .begin_child("Binary")
                .build_expr(&binary.lhs)
                .add_empty_child(binop_str(binary.op))
                .build_expr(&binary.rhs)
                .end_child(),
            ast::Expr::Unary(unary) => unimplemented!(),
        }
    }

    fn print_tree(&mut self) -> std::io::Result<()> {
        let tree = self.builder.build();
        ptree::print_tree(&tree)
    }
}

fn binop_str(op: ast::BinOp) -> &'static str {
    match op {
        ast::BinOp::Add => "+",
        ast::BinOp::Sub => "-",
        ast::BinOp::Mul => "*",
        ast::BinOp::Div => "/",
    }
}

pub fn print(stmt: &ast::Stmt) -> std::io::Result<()> {
    Printer::new().build_stmt(stmt).print_tree()
}
