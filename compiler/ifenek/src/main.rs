use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{AnyValueEnum, BasicValueEnum};
use parse::syntax::ast;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::error::Error;
use typeck::ty;
use typeck::typeck::TypeCk;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    typeck: TypeCk,
}

impl<'ctx> CodeGen<'ctx> {
    fn create(context: &'ctx Context, module: &str) -> Self {
        let module = context.create_module(module);
        let builder = context.create_builder();
        let typeck = TypeCk::new();
        Self {
            context,
            module,
            builder,
            typeck,
        }
    }

    fn add_entry(&mut self) {
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);
    }

    fn compile(&mut self, input: &str) -> Result<(), Box<dyn Error>> {
        let tokens = parse::lex::lex(&input)?;
        let stmt = parse::syntax::parser::parse(tokens.into())?;
        self.typeck.typecheck_stmt(&stmt)?;
        match stmt {
            ast::Stmt::VarDecl(var_decl) => {
                let val = self.compile_var_decl(var_decl);
                self.print_val(val);
            }
            ast::Stmt::Expr(expr) => {
                let val = self.compile_expr(&expr);
                self.print_val(val);
            }
        }
        Ok(())
    }

    fn print_val(&self, val: BasicValueEnum<'ctx>) {
        match val.get_type() {
            BasicTypeEnum::IntType(_) => val.into_int_value().print_to_stderr(),
            _ => unimplemented!(),
        }
        println!();
    }

    fn compile_var_decl(&mut self, var_decl: ast::VarDecl) -> BasicValueEnum<'ctx> {
        let expr_val = self.compile_expr(&var_decl.init);
        let ptr_value = self
            .builder
            .build_alloca(expr_val.get_type(), &var_decl.name.raw);
        self.builder.build_store(ptr_value, expr_val);
        expr_val
    }

    fn compile_expr(&mut self, expr: &ast::Expr) -> BasicValueEnum<'ctx> {
        let value = match expr {
            ast::Expr::Lit(lit) => match lit.kind {
                ast::LitKind::Int(v) => {
                    let ty = self.typeck.get_type(lit.id).unwrap().into_int_ty();
                    let llvm_ty = match ty {
                        ty::IntTy::ISize => self.context.i64_type(),
                        _ => unimplemented!(),
                    };
                    llvm_ty.const_int(v, false).into()
                }
                _ => unimplemented!(),
            },
            _ => {
                unimplemented!()
            }
        };
        value
    }

    // fn get_llvm_ty(&self, ty: &ty::Type) -> AnyTypeEnum<'ctx> {
    //     match ty {
    //         ty::Type::Void => self.context.void_type().into(),
    //         ty::Type::Int(int_ty) => match int_ty {
    //             ty::IntTy::ISize => self.context.i64_type().into(),
    //             _ => unimplemented!(),
    //         },
    //         _ => unimplemented!(),
    //     }
    // }

    // fn get_llvm_basic_ty(&self, ty: &ty::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
    //     match ty {
    //         ty::Type::Int(int_ty) => match int_ty {
    //             ty::IntTy::ISize => Box::new(self.context.i64_type()),
    //             _ => unimplemented!(),
    //         },
    //         _ => unimplemented!(),
    //     }
    // }
}

fn main() -> Result<(), Box<dyn Error>> {
    let history_file = ".ifenek.history";
    let context = Context::create();
    let mut codegen = CodeGen::create(&context, "ifenek");
    let mut rl: Editor<()> = Editor::new();
    if rl.load_history(&history_file).is_err() {}

    codegen.add_entry();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if let Err(err) = codegen.compile(&line) {
                    println!("error: {}", err);
                }
            }
            Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(err) => return Err(Box::new(err)),
        }
    }

    if rl.save_history(&history_file).is_err() {}
    println!("End");
    Ok(())
}
