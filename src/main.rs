use std::collections::HashMap;

use colored::Colorize as _;
use rustyline::{error::ReadlineError, Editor};
use shtml::{env::Env, ns, reader};

fn main() {
    match run() {
        Ok(()) => {}
        Err(e) => eprintln!("{} {e}", "err:".red()),
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = Editor::<(), rustyline::history::DefaultHistory>::new()?;
    if rl.load_history(".mal-history").is_err() {
        eprintln!("No previous history.");
    }

    let mut env = {
        let mut data = HashMap::new();
        ns::std(&mut data);
        Env::top(data)
    };

    loop {
        match rl.readline("user> ") {
            Ok(input) => {
                let _ = rl.add_history_entry(&input);

                match reader::parse(&input) {
                    Ok(Some(value)) => match env.eval(value) {
                        Ok(value) => println!("> {value}"),
                        Err(e) => println!("{e}"),
                    },
                    Ok(None) => {}
                    Err(e) => println!("{e}"),
                };
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("readline error: {err:?}");
                break;
            }
        }

        let _ = rl.save_history(".mal-history");
    }

    Ok(())
}
