#![warn(clippy::pedantic)]

use std::{
    ffi::OsStr,
    fs,
    path::{Path, PathBuf},
};

use ogj_mal::{Env, Error};
use shtml::mal::Config;
use walkdir::WalkDir;

fn main() -> Result<(), anyhow::Error> {
    let path: PathBuf = PathBuf::from("site");
    let dist: PathBuf = PathBuf::from("_site");

    for entry in walkdir(&path)? {
        match entry.as_path().file_name().and_then(OsStr::to_str) {
            Some(s) => {
                if s.starts_with('#') {
                    eprintln!("ignored: {:?}", entry.as_path());
                    continue;
                }
            }
            None => eprintln!("warning: failed to get filename of {:?}", entry.as_path()),
        }

        let project_dir = path.as_path();
        let path = project_dir.join(&entry);
        let dist = dist.join(&entry);

        let contents = fs::read_to_string(&path)?;

        fs::create_dir_all(dist.parent().unwrap_or(PathBuf::new().as_path()))?;
        fs::write(
            dist,
            match transform(
                &Config {
                    project_dir,
                    file_path: &path,
                },
                &contents,
            ) {
                Ok(contents) => contents,
                Err(e) => {
                    eprintln!("failed: {:?}", entry.as_path());
                    eprintln!("{e}\n    in '{}'", path.display());
                    continue;
                }
            },
        )?;

        eprintln!("built: {:?}", entry.as_path());
    }

    eprintln!("site built to {dist:?}");

    Ok(())
}

fn transform(config: &Config, input: &str) -> Result<String, Error> {
    shtml::mal::transform(&Env::default(), config, input)
}

fn walkdir(path: impl AsRef<Path>) -> Result<Vec<PathBuf>, anyhow::Error> {
    let mut vec = vec![];
    for entry in WalkDir::new(&path) {
        let entry = entry?;
        if entry.file_type().is_dir() {
            continue;
        }

        let entry = entry.path().strip_prefix(&path)?;
        vec.push(entry.to_path_buf());
    }
    Ok(vec)
}
