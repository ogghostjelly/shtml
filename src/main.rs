use std::{env::current_dir, rc::Rc};

use clap::Parser as _;
use colored::Colorize as _;
use rustyline::{error::ReadlineError, Editor};
use shtml::{
    cli::{Cli, Commands, ProjectPath},
    env::Env,
    ns,
    reader::{self, Element, Location},
    types::CallContext,
};

fn main() {
    let res = match Cli::parse().commands.unwrap_or(Commands::Repl) {
        Commands::Repl => repl(),
        Commands::Build(path) => build(path),
        Commands::Watch(path) => watch(path),
    };

    match res {
        Ok(()) => {}
        Err(e) => eprintln!("{} {e}", "err:".red()),
    }
}

fn repl() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = Editor::<(), rustyline::history::DefaultHistory>::new()?;
    if rl.load_history(".mal-history").is_err() {
        eprintln!("No previous history.");
    }

    let mut env = {
        let mut data = Env::empty();
        ns::std(&mut data);
        data
    };

    let loc = Location::new(Rc::new("repl".into()), 1, 1);
    let src = CallContext::repl(current_dir()?);

    loop {
        match rl.readline("user> ") {
            Ok(input) => {
                let _ = rl.add_history_entry(&input);

                match reader::parse_file(loc.clone(), &input) {
                    Ok(vals) => {
                        for value in vals {
                            match value {
                                Element::Text(value) => print!("{value}"),
                                Element::Value(value) => match env.eval(&src, value) {
                                    Ok(value) => print!("{}", value.value),
                                    Err(e) => println!("{e}"),
                                },
                            }
                        }
                        println!()
                    }
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

fn watch(path: ProjectPath) -> Result<(), Box<dyn std::error::Error>> {
    let path = path.or_pwd()?;

    _ = path;

    todo!()
}

fn build(path: ProjectPath) -> Result<(), Box<dyn std::error::Error>> {
    let path = path.or_pwd()?;

    _ = path;

    todo!()
}
