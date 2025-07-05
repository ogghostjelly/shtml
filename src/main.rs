use colored::Colorize as _;
use pest::Parser;
use rustyline::{error::ReadlineError, Editor};
use shtml::reader;

fn read(input: String) -> String {
    input
}

fn eval(input: String) -> String {
    input
}

fn print(input: String) -> String {
    input
}

fn rep(input: String) -> String {
    print(eval(read(input)))
}

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

    loop {
        match rl.readline("user> ") {
            Ok(input) => {
                let _ = rl.add_history_entry(&input);

                match reader::parse(&input) {
                    Ok(Some(expr)) => {
                        println!("{expr:?}\n{expr}")
                    }
                    Ok(None) => {}
                    Err(e) => println!("{e}"),
                };
                println!("> {}", rep(input));
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
