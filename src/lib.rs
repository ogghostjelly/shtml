use reader::Location;
use types::MalData;

pub mod cli;
pub mod env;
pub mod ns;
pub mod printer;
pub mod reader;
pub mod types;

pub type MalRet = Result<MalData, Error>;

#[derive(thiserror::Error, Debug)]
#[error("{kind} at {loc}")]
pub struct Error {
    kind: ErrorKind,
    loc: Location,
}

impl Error {
    pub fn new(kind: ErrorKind, loc: Location) -> Self {
        Self { kind, loc }
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
    #[error("expected type '{0}' but got '{1}' in {2}")]
    UnexpectedType(&'static str, &'static str, String),
    #[error("'{0}' expects an even number of arguments")]
    UnevenArguments(&'static str),
    #[error("'{0}' cannot be a map key")]
    InvalidMapKey(&'static str),
    #[error("expected {0} arguments but got {1} in {2}")]
    ArityMismatch(usize, usize, String),
    #[error("expected at least {0} arguments but got {1} in {2}")]
    AtleastArityMismatch(usize, usize, String),
    #[error("cannot have more binds after variadic '&'")]
    BindsAfterRest,
    #[error("symbol not found '{0}' cannot be used outside of quasiquote")]
    OutsideOfQuasiquote(&'static str),
    #[error("index out of range {0} for list of size {1}")]
    IndexOutOfRange(i64, usize),
    #[error("cannot use 'first' on an empty list")]
    FirstOfEmptyList,
}
