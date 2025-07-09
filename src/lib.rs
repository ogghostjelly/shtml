use std::{fmt, path::PathBuf};

use reader::Location;
use types::{CallContext, MalData};

pub mod cli;
pub mod env;
pub mod ns;
pub mod printer;
pub mod reader;
pub mod types;

pub type MalRet = Result<MalData, Error>;

#[derive(thiserror::Error, Debug)]
pub struct Error {
    kind: ErrorKind,
    loc: Location,
    frames: Vec<(String, Location)>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.kind, self.loc)?;
        for (name, loc) in self.frames.iter().rev() {
            write!(f, "\n   in {name} at {loc}")?;
        }
        Ok(())
    }
}

impl Error {
    pub fn new(kind: ErrorKind, ctx: &CallContext, loc: Location) -> Self {
        Self {
            frames: ctx.frames(),
            kind,
            loc,
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ErrorKind {
    #[error("symbol not found '{0}'")]
    NotFound(String),
    #[error("cannot apply '{0}'")]
    CannotApply(&'static str),
    #[error("cannot use '{op}' on '{fst}' and '{snd}'")]
    InvalidOperation {
        op: &'static str,
        fst: &'static str,
        snd: &'static str,
    },
    #[error("cannot use '{0}' on '{1}'")]
    InvalidOperation1(&'static str, &'static str),
    #[error("expected type '{0}' but got '{1}'")]
    UnexpectedType(&'static str, &'static str),
    #[error("'{0}' expects an even number of arguments")]
    UnevenArguments(&'static str),
    #[error("'{0}' cannot be a map key")]
    InvalidMapKey(&'static str),
    #[error("expected {0} arguments but got {1}")]
    ArityMismatch(usize, usize),
    #[error("expected at least {0} arguments but got {1}")]
    AtleastArityMismatch(usize, usize),
    #[error("cannot have more binds after variadic '&'")]
    BindsAfterRest,
    #[error("symbol not found '{0}' cannot be used outside of quasiquote")]
    OutsideOfQuasiquote(&'static str),
    #[error("index out of range {0} for list of size {1}")]
    IndexOutOfRange(i64, usize),
    #[error("cannot use 'first' on an empty list")]
    FirstOfEmptyList,
    #[error("denied access to filesystem, you cannot use the filesystem functions")]
    DeniedFsAccess,
    #[error("invalid path '{}'", _0.display())]
    InvalidPath(PathBuf),
}
