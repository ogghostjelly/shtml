use std::{
    env::current_dir,
    fs,
    io::{self, Cursor},
    path::{Path, PathBuf},
    rc::Rc,
    sync::mpsc,
    time::Duration,
};

use clap::Parser as _;
use colored::Colorize as _;
use notify_debouncer_full::new_debouncer;
use notify_debouncer_full::notify;
use rustyline::{error::ReadlineError, Editor};
use shtml::{
    cli::{Cli, Commands, ProjectPath},
    env::Env,
    load,
    reader::{self, Location},
    types::{CallContext, List, MalData, MalVal},
};

fn main() {
    let res = match Cli::parse()
        .commands
        .unwrap_or(Commands::Watch(ProjectPath::default()))
    {
        Commands::Repl => repl(),
        Commands::Build(path) => build(path),
        Commands::Watch(path) => convert_error(watch(path)),
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

    let mut env = Env::std();

    let loc = Location::new(Rc::new("repl".into()), 1, 1);
    let src = CallContext::repl(current_dir()?);

    loop {
        match rl.readline("user> ") {
            Ok(input) => {
                let _ = rl.add_history_entry(&input);

                match reader::parse_str(loc.clone(), &input) {
                    Ok(vals) => {
                        for value in vals {
                            match env.eval(&src, value) {
                                Ok(value) => println!("> {}", value.value),
                                Err(e) => println!("err: {e}"),
                            }
                        }
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

fn convert_error<T, E: std::error::Error + 'static>(
    res: Result<T, E>,
) -> Result<T, Box<dyn std::error::Error>> {
    Ok(res?)
}

fn watch(project_path: ProjectPath) -> Result<(), Error> {
    let path = project_path
        .clone()
        .or_pwd()
        .map_err(Error::GetPwd)?
        .canonicalize()
        .map_err(Error::GetPwd)?;
    let dist = path.join("_site");

    let (tx, rx) = mpsc::channel();
    let mut debouncer = new_debouncer(Duration::from_millis(200), None, tx)?;
    debouncer.watch(path, notify::RecursiveMode::Recursive)?;

    match build(project_path.clone()) {
        Ok(()) => {}
        Err(e) => eprintln!("{} {e}", "err:".red()),
    }

    for res in rx {
        match res {
            Ok(e) => {
                let changed = e.into_iter().find(|e| match e.kind {
                    notify::EventKind::Create(_)
                    | notify::EventKind::Modify(_)
                    | notify::EventKind::Remove(_) => !e.paths.iter().all(|p| p.starts_with(&dist)),
                    notify::EventKind::Access(_)
                    | notify::EventKind::Any
                    | notify::EventKind::Other => false,
                });

                if changed.is_some() {
                    println!(
                        "[{:05}] Rebuilding",
                        std::time::UNIX_EPOCH
                            .elapsed()
                            .expect("clock should not be before UNIX EPOCH")
                            .as_millis()
                            % 100000, // get the last few digits
                    );

                    match build(project_path.clone()) {
                        Ok(()) => {}
                        Err(e) => eprintln!("{} {e}", "err:".red()),
                    }
                }
            }
            Err(errs) => {
                for e in errs {
                    eprintln!("{} {e}", "err:".red());
                }
            }
        }
    }

    Ok(())
}

fn build(path: ProjectPath) -> Result<(), Box<dyn std::error::Error>> {
    let path = path.or_pwd().map_err(Error::GetPwd)?;
    let path = Rc::new(
        path.canonicalize()
            .map_err(|e| Error::FindProjectDir(path, e))?,
    );

    match build_inner(Rc::clone(&path)) {
        Ok(()) => Ok(()),
        Err(e) => {
            _ = fs::remove_dir_all(path.join("_site"));
            Err(e.into())
        }
    }
}

fn build_inner(path: Rc<PathBuf>) -> Result<(), Error> {
    let mut env = Env::std();
    let _ = eval(&mut env, Rc::clone(&path), "config.mal")?;

    let dist = path.join("_site");
    match fs::remove_dir_all(&dist) {
        Ok(()) => {}
        Err(e) if matches!(e.kind(), io::ErrorKind::NotFound) => {}
        Err(e) => return Err(Error::RemoveDir("_site".to_string(), e)),
    }
    fs::create_dir(&dist).map_err(|e| Error::CreateDir("_site".to_string(), e))?;

    rec_dir(&env, path, &dist, "", |filename| {
        filename == "_site" || filename == "config.mal"
    })
}

fn rec_dir(
    env: &Env,
    from: Rc<PathBuf>,
    to: &Path,
    dir: &str,
    ignore: impl Fn(&str) -> bool,
) -> Result<(), Error> {
    for entry in read_dir(&from, dir)? {
        let entry = entry.map_err(|e| Error::ReadDir(dir.to_string(), e))?;
        let path = entry.path();

        let filename = entry.file_name();
        let filename = filename
            .to_str()
            .ok_or(Error::MalformedPath(path.clone()))?;
        if filename.starts_with(".")
            || filename.starts_with("#")
            || filename.ends_with(".mal")
            || ignore(filename)
        {
            continue;
        }

        let metadata = entry
            .metadata()
            .map_err(|e| Error::ReadDir(dir.to_string(), e))?;

        let rel_path = if dir.is_empty() {
            filename.to_string()
        } else {
            format!("{dir}/{filename}")
        };
        let out = to.join(&rel_path);

        if metadata.is_dir() {
            fs::create_dir(out).map_err(|e| Error::CreateDir(dir.to_string(), e))?;
            rec_dir(env, Rc::clone(&from), to, &rel_path, allow)?;
        } else if metadata.is_file() {
            if filename.ends_with(".shtml") {
                build_shtml_file(env, Rc::clone(&from), to, &rel_path)?;
            } else {
                fs::copy(path, out).map_err(|e| Error::CopySiteFile(rel_path, e))?;
            }
        }
    }

    Ok(())
}

fn build_shtml_file(env: &Env, from: Rc<PathBuf>, to: &Path, path: &str) -> Result<(), Error> {
    let mut out = fs::File::create_new(to.join(path).with_extension("html"))
        .map_err(|e| Error::CreateFile(path.to_string(), e))?;

    let ctx = CallContext::with_fs(env.clone(), from.clone(), path);
    let mut env = env.clone();

    let text = load::shtml(&ctx, &mut env, path, from.join(path)).map_err(Error::LoadShtml)?;
    _ = io::copy(&mut Cursor::new(text), &mut out)
        .map_err(|e| Error::CopySiteFile(path.to_string(), e))?;

    Ok(())
}

fn allow(_: &str) -> bool {
    false
}

fn read_dir(base: &Path, dir: &str) -> Result<std::fs::ReadDir, Error> {
    let path = base.join(dir);
    path.read_dir()
        .map_err(|e| Error::ReadDir(dir.to_string(), e))
}

fn eval(env: &mut Env, root: Rc<PathBuf>, file: &str) -> Result<Rc<MalData>, Error> {
    let ast = parse_mal(&root, file)?;
    let ctx = CallContext::with_fs(env.clone(), root, file);
    Ok(env.eval(&ctx, ast)?)
}

fn read_file(base: &Path, file: &str) -> Result<fs::File, Error> {
    let path = base.join(file);
    fs::File::open(path).map_err(|e| Error::ReadFile(file.to_string(), e))
}

fn parse_mal(base: &Path, file: &str) -> Result<Rc<MalData>, Error> {
    let contents = read_file(base, file)?;
    let loc = Location::file(file);
    let vec = reader::parse_reader(loc.clone(), contents)
        .map_err(|e| Error::ParseFile(file.to_string(), e.to_string()))?;

    let mut ls = List::from_vec(vec);
    ls.push_front(MalVal::Sym("do".to_string()).with_loc(loc.clone()));
    Ok(MalVal::List(ls).with_loc(loc))
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("couldn't find the project directory, '{0}'")]
    FindProjectDir(PathBuf, io::Error),
    #[error("couldn't get the current directory: {0}")]
    GetPwd(io::Error),
    #[error("couldn't parse '{0}': {1}")]
    ParseFile(String, String),
    #[error("couldn't read '{0}': {1}")]
    ReadFile(String, io::Error),
    #[error("couldn't read dir '{0}': {1}")]
    ReadDir(String, io::Error),
    #[error("couldn't create dir '{0}': {1}")]
    CreateDir(String, io::Error),
    #[error("couldn't create file '{0}': {1}")]
    CreateFile(String, io::Error),
    #[error("couldn't remove dir '{0}': {1}")]
    RemoveDir(String, io::Error),
    #[error(transparent)]
    LoadShtml(#[from] shtml::load::Error),
    #[error(transparent)]
    Shtml(#[from] shtml::Error),
    #[error("invalid UTF-8 in path, '{0}'")]
    MalformedPath(PathBuf),
    #[error("couldn't copy '{0}' to '_site': {1}")]
    CopySiteFile(String, io::Error),
    #[error("shtml watch: notify: {0}")]
    Notify(#[from] notify::Error),
}
