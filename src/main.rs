#![warn(clippy::pedantic)]

use std::{
    fs,
    path::{Path, PathBuf},
};

use ogj_mal::Error;
use walkdir::WalkDir;

fn main() -> Result<(), anyhow::Error> {
    let path: PathBuf = PathBuf::from("site");
    let dist: PathBuf = PathBuf::from("_site");

    for entry in walkdir(&path)? {
        let path = path.join(&entry);
        let dist = dist.join(&entry);

        let contents = fs::read_to_string(&path)?;

        fs::create_dir_all(dist.parent().unwrap_or(PathBuf::new().as_path()))?;
        fs::write(
            dist,
            match transform(&contents) {
                Ok(contents) => contents,
                Err(e) => {
                    eprintln!("{e}\n    in '{}'", path.display());
                    continue;
                }
            },
        )?;
    }

    Ok(())
}

fn transform(input: &str) -> Result<String, Error> {
    shtml::mal::transform(input)
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
