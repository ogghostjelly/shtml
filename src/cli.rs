use std::{env::current_dir, io, path::PathBuf};

use clap::{Args, Parser, Subcommand};

#[derive(Parser)]
pub struct Cli {
    #[clap(subcommand)]
    pub commands: Option<Commands>,
}
#[derive(Subcommand, Clone)]
pub enum Commands {
    Repl,
    Build(ProjectPath),
    Watch(ProjectPath),
}

#[derive(Args, Clone, Default)]
pub struct ProjectPath {
    /// The path to a root project directory or the current directory if not specified.
    path: Option<PathBuf>,
}

impl ProjectPath {
    pub fn or_pwd(self) -> io::Result<PathBuf> {
        match self.path {
            Some(path) => Ok(path),
            None => current_dir(),
        }
    }
}
