use clap::Clap;
use opts::Opts;
use std::error::Error;
use std::io::prelude::Read;
use std::path::Path;

mod opts;

fn main() {
    if let Err(err) = run_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn read_file<P: AsRef<Path> + std::fmt::Display>(src: P) -> std::io::Result<String> {
    let mut src_file = std::fs::File::open(&src)?;
    let mut buf = String::new();
    src_file.read_to_string(&mut buf)?;
    Ok(buf)
}

fn run_main() -> Result<(), Box<dyn Error>> {
    let opts = Opts::parse();
    let input = read_file(&opts.src).map_err(|err| format!("{}: {}", &opts.src, err))?;
    let stmts = parse::parse(&input)?;
    let mut typeck = typeck::TypeCk::new();
    let mir = stmts
        .into_iter()
        .map(|stmt| typeck.typecheck_stmt(&stmt))
        .collect::<Result<Vec<typeck::mir::Stmt>, _>>()?;
    if opts.emit.contains(&opts::Emit::Ast) {
        printer::print(mir.as_slice())?;
    }
    Ok(())
}
