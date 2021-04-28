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

fn run_main() -> Result<(), Box<dyn Error>> {
    let opts = Opts::parse();
    let src = SrcFile::open(opts.src)?;
    let tokens = lex::lex(&src)?;
    let stmt = parse::parse(&src, tokens.into())?;
    printer::ast::print_stmt(&stmt)?;

    // let mir_fun = {
    //     let input = read_file(&opts.src).map_err(|err| format!("{}: {}", &opts.src, err))?;
    //     let ast_arena = parse::ast::AstArena::new();
    //     let fun = parse::parse(&input, &ast_arena)?;
    //     typeck::lower(&fun)?
    // };
    // if opts.emit.contains(&opts::Emit::Ast) {
    //     printer::print(&mir_fun)?;
    // }

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
