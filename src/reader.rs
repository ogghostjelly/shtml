use std::{io, rc::Rc};

use derive_more::Display;

use crate::types::MalData;

mod internal;
mod parser;
pub use parser::is_valid_char;

pub fn parse_str(loc: Location, s: &str) -> Result<Vec<Rc<MalData>>> {
    let r = internal::Reader::from_chars(s.chars(), loc);
    try_reader(r, |r| r.parse_file())
}

pub fn parse_reader<R>(loc: Location, reader: R) -> Result<Vec<Rc<MalData>>>
where
    R: io::Read,
{
    let r = internal::Reader::from_unicode_reader(reader, loc);
    try_reader(r, |r| r.parse_file())
}

fn try_reader<I, T>(
    mut reader: internal::Reader<I>,
    f: impl FnOnce(&mut internal::Reader<I>) -> parser::Result<T>,
) -> Result<T>
where
    I: Iterator<Item = parser::Result<char>>,
{
    match f(&mut reader) {
        Ok(value) => Ok(value),
        Err(e) => Err(Error {
            loc: reader.finalize(),
            inner: e,
        }),
    }
}

#[derive(Display, Debug, Clone, PartialEq)]
#[display("{line}:{col} in {file}")]
pub struct Location {
    file: Rc<String>,
    line: usize,
    col: usize,
}

impl Location {
    pub fn new(file: Rc<String>, line: usize, col: usize) -> Self {
        Self { file, line, col }
    }

    pub fn file(file: impl Into<String>) -> Self {
        Self {
            file: Rc::new(file.into()),
            line: 1,
            col: 1,
        }
    }
}

#[macro_export]
macro_rules! loc {
    () => {{
        use std::rc::Rc;
        $crate::reader::Location::new(
            Rc::new(file!().into()),
            line!() as usize,
            column!() as usize,
        )
    }};
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
#[error("{inner} at {loc}")]
pub struct Error {
    loc: Location,
    inner: parser::Error,
}
