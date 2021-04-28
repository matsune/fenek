// use parse::ast;
// use ptree::TreeBuilder;
// use typeck::hir;
// use typeck::hir::Typed;
// use typeck::ty;

// pub fn print(fun: &hir::Fun) -> std::io::Result<()> {
//     let mut printer = Printer::new();
//     printer.build_fun(fun);
//     printer.print_tree()
// }

// struct Printer {
//     builder: TreeBuilder,
// }

// impl Printer {
//     fn new() -> Self {
//         Self {
//             builder: TreeBuilder::new("".to_string()),
//         }
//     }

//     fn begin_child(&mut self, name: &str) -> &mut Self {
//         self.builder.begin_child(name.to_string());
//         self
//     }

//     fn end_child(&mut self) -> &mut Self {
//         self.builder.end_child();
//         self
//     }

//     fn add_empty_child(&mut self, name: &str) -> &mut Self {
//         self.builder.add_empty_child(name.to_string());
//         self
//     }

//     fn build_fun(&mut self, fun: &hir::Fun) -> &mut Self {
//         let args = fun
//             .args
//             .iter()
//             .map(|arg| format!("{}: {:?}", arg.raw, arg.def.as_var_def().ty))
//             .collect::<Vec<_>>()
//             .join(", ");
//         let ret_ty = match fun.ret_ty {
//             ty::Type::Void => String::from(""),
//             _ => format!(" -> {:?}", fun.ret_ty),
//         };
//         self.begin_child(&format!("fun {}({}){}", fun.name.raw, args, ret_ty));
//         for stmt in fun.block.stmts.iter() {
//             self.build_stmt(stmt);
//         }
//         self
//     }

//     fn build_stmt(&mut self, stmt: &hir::Stmt) -> &mut Self {
//         match stmt {
//             hir::Stmt::VarDecl(var_decl) => self
//                 .begin_child("VarDecl")
//                 .add_empty_child(&format!(
//                     "{}::{:?} def_id={}",
//                     var_decl.name.raw, var_decl.def.ty, var_decl.def.id,
//                 ))
//                 .add_empty_child("=")
//                 .build_expr(&var_decl.init)
//                 .end_child(),
//             hir::Stmt::Expr(expr) => self.build_expr(expr),
//             hir::Stmt::Ret(ret) => {
//                 self.begin_child("Ret");
//                 match &ret.expr {
//                     Some(expr) => {
//                         self.build_expr(&expr);
//                     }
//                     None => {}
//                 };
//                 self.end_child()
//             }
//         }
//     }

//     fn build_expr(&mut self, expr: &hir::Expr) -> &mut Self {
//         let ty = expr.get_type();
//         match expr {
//             hir::Expr::Lit(lit) => match &lit.kind {
//                 hir::LitKind::I8(v) => self.add_empty_child(&format!("{}::i8", v)),
//                 hir::LitKind::I16(v) => self.add_empty_child(&format!("{}::i16", v)),
//                 hir::LitKind::I32(v) => self.add_empty_child(&format!("{}::i32", v)),
//                 hir::LitKind::I64(v) => self.add_empty_child(&format!("{}::i64", v)),
//                 hir::LitKind::F32(v) => self.add_empty_child(&format!("{}::f32", v)),
//                 hir::LitKind::F64(v) => self.add_empty_child(&format!("{}::f64", v)),
//                 hir::LitKind::Bool(v) => self.add_empty_child(&format!("{}::bool", v)),
//             },
//             hir::Expr::Ident(ident) => self.add_empty_child(&format!(
//                 "{}::{:?} def_id={}",
//                 ident.raw,
//                 ty,
//                 ident.def.id()
//             )),
//             hir::Expr::Binary(binary) => self
//                 .begin_child(&format!("Binary::{:?}", ty))
//                 .build_expr(&binary.lhs)
//                 .add_empty_child(&binary.op)
//                 .build_expr(&binary.rhs)
//                 .end_child(),
//             hir::Expr::Unary(unary) => self
//                 .begin_child(&format!("Unary::{:?}", ty))
//                 .add_empty_child(&format!("{:?}", unary.op))
//                 .build_expr(&unary.expr)
//                 .end_child(),
//         }
//     }

//     fn print_tree(&mut self) -> std::io::Result<()> {
//         let tree = self.builder.build();
//         ptree::print_tree(&tree)
//     }
// }
