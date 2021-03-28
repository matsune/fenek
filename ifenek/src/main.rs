use codegen::CodeGen;
use inkwell::context::Context;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::error::Error;

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
