use clap::Clap;
use codegen::Codegen;
use error::Result;
use inkwell::context::Context;
use opts::Opts;
use pos::SrcFile;
use std::error::Error;

mod opts;

fn main() {
    if let Err(err) = run_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn parse_ast(src: &SrcFile) -> Result<ast::Module> {
    let tokens = lex::lex(&src)?;
    parse::parse(&src, tokens.into())
}

fn run_main() -> Result<(), Box<dyn Error>> {
    let opts = Opts::parse();
    let src = SrcFile::open(opts.src)?;
    let module = {
        let ast = parse_ast(&src)?;
        if opts.emit.contains(&opts::Emit::Ast) {
            printer::ast::print(&ast)?;
        }
        typeck::lower(&src, ast)?
    };
    if opts.emit.contains(&opts::Emit::Hir) {
        printer::hir::print(&module)?;
    }

    let ctx = Context::create();
    let mut codegen = Codegen::new(&ctx);
    codegen.build_module(module);
    if opts.emit.contains(&opts::Emit::LlvmIr) {
        let mut out = src.path;
        out.set_extension("ll");
        codegen.output_to_file(out)?;
    }

    Ok(())
}
