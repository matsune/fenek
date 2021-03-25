use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::{AnyTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{AnyValueEnum, BasicValueEnum};
use inkwell::OptimizationLevel;
use parse::syntax::ast;
use std::error::Error;
use std::io::{self, Write};
use typeck::ty;
use typeck::typeck::TypeCk;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    engine: ExecutionEngine<'ctx>,
    typeck: TypeCk,
}

type MainFunc = unsafe extern "C" fn() -> i64;

impl<'ctx> CodeGen<'ctx> {
    fn create(context: &'ctx Context, module: &str) -> Result<Self, LLVMString> {
        let module = context.create_module(module);
        let builder = context.create_builder();
        let engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
        let typeck = TypeCk::new();
        Ok(Self {
            context,
            module,
            builder,
            engine,
            typeck,
        })
    }

    fn add_entry(&mut self) {
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(basic_block);
    }

    fn jit_compile(&mut self, input: &str) -> Result<(), Box<dyn Error>> {
        let tokens = parse::lexer::lex(&input)?;
        let stmt = parse::syntax::parser::parse(tokens.into())?;
        self.typeck.typecheck_stmt(&stmt).unwrap();
        match stmt {
            ast::Stmt::VarDecl(var_decl) => {
                let val = self.jit_compile_var_decl(var_decl)?;
                self.print_val(val);
            }
            ast::Stmt::Expr(expr) => {
                unimplemented!()
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

    fn jit_compile_var_decl(
        &mut self,
        var_decl: ast::VarDecl,
    ) -> Result<BasicValueEnum<'ctx>, Box<dyn Error>> {
        let expr_val = self.jit_compile_expr(&var_decl.init)?;
        let def = self
            .typeck
            .current_scope()
            .lookup(&var_decl.name.raw)
            .unwrap()
            .into_var_def();
        let ptr_value = self
            .builder
            .build_alloca(expr_val.get_type(), &var_decl.name.raw);
        self.builder.build_store(ptr_value, expr_val);
        Ok(expr_val)
    }

    fn jit_compile_expr(
        &mut self,
        expr: &ast::Expr,
    ) -> Result<BasicValueEnum<'ctx>, Box<dyn Error>> {
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
        Ok(value)
    }

    fn get_llvm_ty(&self, ty: &ty::Type) -> AnyTypeEnum<'ctx> {
        match ty {
            ty::Type::Void => self.context.void_type().into(),
            ty::Type::Int(int_ty) => match int_ty {
                ty::IntTy::ISize => self.context.i64_type().into(),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn get_llvm_basic_ty(&self, ty: &ty::Type) -> Box<dyn BasicType<'ctx> + 'ctx> {
        match ty {
            ty::Type::Int(int_ty) => match int_ty {
                ty::IntTy::ISize => Box::new(self.context.i64_type()),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
}

struct Repl<'ctx> {
    codegen: CodeGen<'ctx>,
}

impl<'ctx> Repl<'ctx> {
    fn new(codegen: CodeGen<'ctx>) -> Self {
        Self { codegen }
    }

    fn read_line(&self) -> std::io::Result<String> {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        Ok(input)
    }

    fn run(&mut self) -> Result<(), Box<dyn Error>> {
        self.codegen.add_entry();
        loop {
            let input = self.read_line()?;
            match self.codegen.jit_compile(&input) {
                Ok(()) => {}
                Err(err) => println!("error: {}", err),
            }
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let codegen = CodeGen::create(&context, "ifenek")?;
    Repl::new(codegen).run()?;
    Ok(())
}
