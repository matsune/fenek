use clap::Clap;
use codegen::ModuleBuilder;
use error::Result;
use inkwell::context::Context;
use opts::Opts;
use pos::SrcFile;
use std::error::Error;
use std::path::Path;
use std::process::Command;

mod opts;

fn main() {
    if let Err(err) = run_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn parse_ast(src: &SrcFile) -> Result<ast::Module> {
    let tokens = lex::lex(src.chars())?;
    parse::parse(tokens.into())
}

pub fn print<S: serde::Serialize>(s: S) -> serde_json::Result<()> {
    let json = serde_json::to_string(&s)?;
    println!("{}", json);
    Ok(())
}

fn run_main() -> Result<(), Box<dyn Error>> {
    let opts = Opts::parse();
    let src = SrcFile::open(opts.src)?;
    let module = {
        let ast = parse_ast(&src)?;
        if opts.emit.contains(&opts::Emit::Ast) {
            print(&ast)?;
        }
        typeck::lower(ast)?
    };
    if opts.emit.contains(&opts::Emit::Hir) {
        print(&module)?;
    }

    let ctx = Context::create();
    let mut builder = ModuleBuilder::new(&ctx, "fenec");
    builder.build_module(module)?;
    if opts.emit.contains(&opts::Emit::LlvmIr) {
        let mut out = src.path.clone();
        out.set_extension("ll");
        builder.emit_llvm_ir(out)?;
    }
    if opts.emit.contains(&opts::Emit::LlvmBc) {
        let mut out = src.path.clone();
        out.set_extension("bc");
        builder.emit_llvm_bc(out);
    }
    if opts.emit.contains(&opts::Emit::Asm) {
        let mut out = src.path.clone();
        out.set_extension("s");
        builder.emit_asm(out)?;
    }
    if opts.emit.contains(&opts::Emit::Obj) {
        let mut out = src.path.clone();
        out.set_extension("o");
        builder.emit_obj(out)?;
    }
    if opts.emit.contains(&opts::Emit::Link) {
        let mut obj_path = src.path.clone();
        obj_path.set_extension("o");
        if !opts.emit.contains(&opts::Emit::Obj) {
            builder.emit_obj(obj_path.clone())?;
        }
        let mut out = src.path;
        out.set_extension("");
        Command::new("gcc")
            .args(&["-o", out.to_str().unwrap(), &obj_path.to_str().unwrap()])
            .output()
            .expect("failed to link obj files");
        if !opts.emit.contains(&opts::Emit::Obj) {
            std::fs::remove_file(obj_path).expect("failed to remove obj file");
        }
    }
    Ok(())
}
