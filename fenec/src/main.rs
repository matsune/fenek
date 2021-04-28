use clap::Clap;
// use codegen::Codegen;
use inkwell::context::Context;
use opts::Opts;
use span::SrcFile;
use std::error::Error;
use std::io::prelude::Read;
use std::path::{Path, PathBuf};

mod opts;

fn main() {
    if let Err(err) = run_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn parse_ast(src: &SrcFile) -> Result<ast::Fun, error::CompileError> {
    let tokens = lex::lex(&src)?;
    parse::parse(&src, tokens.into())
}

fn run_main() -> Result<(), Box<dyn Error>> {
    let opts = Opts::parse();
    let src = SrcFile::open(opts.src)?;
    let hir_fun = {
        let ast_fun = parse_ast(&src)?;
        if opts.emit.contains(&opts::Emit::Ast) {
            printer::ast::print_fun(&ast_fun)?;
        }
        typeck::lower(&src, ast_fun)?
    };
    if opts.emit.contains(&opts::Emit::Hir) {
        // TODO
        // printer::hir::print_fun(&hir_fun)?;
    }

    // let ctx = Context::create();
    // let mut codegen = Codegen::new(&ctx);
    // codegen.build_fun(&mir_fun);
    // if opts.emit.contains(&opts::Emit::LlvmIr) {
    //     let mut out = PathBuf::from(opts.src);
    //     out.set_extension("ll");
    //     codegen.output_to_file(out)?;
    // }

    Ok(())
}
