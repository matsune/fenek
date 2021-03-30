use inkwell::context::Context;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::error::Error;

mod repl;

use repl::Repl;

fn main() -> Result<(), Box<dyn Error>> {
    let history_file = ".ifenek.history";
    let ctx = Context::create();
    let mut repl = Repl::create(&ctx, "ifenek")?;
    let mut rl: Editor<()> = Editor::new();
    if rl.load_history(&history_file).is_err() {}

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if let Err(err) = repl.eval(&line) {
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
