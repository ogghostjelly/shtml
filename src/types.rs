use std::{
    fmt, iter,
    path::{Path, PathBuf},
    rc::Rc,
    slice, vec,
};

use indexmap::IndexMap;

use crate::{
    env::{Env, TcoRet},
    reader::Location,
    MalRet,
};

#[derive(Debug, Clone)]
pub struct MalData {
    pub value: MalVal,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum MalVal {
    List(List),
    Vector(Vec<Rc<MalData>>),
    Map(IndexMap<MalKey, Rc<MalData>>),
    Sym(String),
    Str(String),
    Kwd(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    BuiltinFn(String, fn(&CallContext, (List, Location)) -> MalRet),
    Special(
        String,
        fn(&CallContext, &mut Env, (List, Location)) -> TcoRet,
    ),
    Fn(MalFn),
    Env(Env),
    Nil,
    Html(Html),
}

impl MalData {
    pub fn is_true(&self) -> bool {
        match &self.value {
            MalVal::Nil => false,
            MalVal::Bool(value) => *value,
            _ => true,
        }
    }

    pub fn type_name(&self) -> &'static str {
        self.value.type_name()
    }
}

impl MalVal {
    pub fn with_loc(self, loc: Location) -> Rc<MalData> {
        Rc::new(MalData { value: self, loc })
    }

    pub const LIST: &str = "list";
    pub const VECTOR: &str = "vector";
    pub const LIST_LIKE: &str = "list-like";
    pub const HASH_MAP: &str = "hash-map";
    pub const SYM: &str = "symbol";
    pub const STR: &str = "string";
    pub const KWD: &str = "keyword";
    pub const INT: &str = "int";
    pub const FLOAT: &str = "float";
    pub const BOOL: &str = "bool";
    pub const FN: &str = "function";
    pub const MACRO: &str = "macro";
    pub const ENV: &str = "env";
    pub const NIL: &str = "nil";
    pub const HTML: &str = "html";

    pub fn type_name(&self) -> &'static str {
        match self {
            MalVal::List(_) => Self::LIST,
            MalVal::Vector(_) => Self::VECTOR,
            MalVal::Map(_) => Self::HASH_MAP,
            MalVal::Sym(_) => Self::SYM,
            MalVal::Str(_) => Self::STR,
            MalVal::Kwd(_) => Self::KWD,
            MalVal::Int(_) => Self::INT,
            MalVal::Float(_) => Self::FLOAT,
            MalVal::Bool(_) => Self::BOOL,
            MalVal::BuiltinFn(_, _) | MalVal::Fn { .. } => Self::FN,
            MalVal::Special(_, _) => Self::MACRO,
            MalVal::Env(_) => Self::ENV,
            MalVal::Nil => Self::NIL,
            MalVal::Html(_) => Self::HTML,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MalFn {
    pub name: Option<String>,
    pub is_macro: bool,
    pub outer: Env,
    pub binds: Vec<String>,
    pub bind_rest: Option<Option<(Location, String)>>,
    pub body: (Rc<MalData>, List),
}

#[derive(Debug, Clone)]
pub struct List(Vec<Rc<MalData>>);

impl Default for List {
    fn default() -> Self {
        Self::new()
    }
}

impl List {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn first(&self) -> Option<&Rc<MalData>> {
        self.0.last()
    }

    pub fn swap_remove(&mut self, index: usize) -> Rc<MalData> {
        self.0.swap_remove(self.0.len() - index - 1)
    }

    pub fn split_off(&mut self, at: usize) -> Self {
        let mut o = self.0.split_off(self.len() - at);
        std::mem::swap(&mut self.0, &mut o);
        Self(o)
    }

    pub fn into_array<const N: usize>(self) -> Result<[Rc<MalData>; N], List> {
        self.0.try_into().map_err(List).map(|mut arr: [_; N]| {
            arr.reverse();
            arr
        })
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> iter::Rev<slice::Iter<Rc<MalData>>> {
        self.0.iter().rev()
    }

    pub fn pop_front(&mut self) -> Option<Rc<MalData>> {
        self.0.pop()
    }

    pub fn push_front(&mut self, value: Rc<MalData>) {
        self.0.push(value);
    }
}

impl IntoIterator for List {
    type Item = Rc<MalData>;

    type IntoIter = iter::Rev<vec::IntoIter<Rc<MalData>>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter().rev()
    }
}

#[macro_export]
macro_rules! list {
    () => (
        $crate::types::MalVal::List(
        $crate::types::List::from_rev(std::vec::Vec::new()))
    );

    ([] reversed: $x:expr) => {
        $crate::types::MalVal::List(
        $crate::types::List::from_rev(<[_]>::into_vec(
            // Using the intrinsic produces a dramatic improvement in stack usage for
            // unoptimized programs using this code path to construct large Vecs.
            std::boxed::Box::new($x)
        )))
    };

    ($($x:expr),* $(,)?) => {
        list!([$($x),*] reversed: [])
    };

    ([$head:expr $(, $tail:expr)*] reversed: [$($reversed:expr),*]) => {
        list!([$($tail),*] reversed: [$head $(, $reversed)*])
    };
}

impl List {
    pub fn from_rev(rev: Vec<Rc<MalData>>) -> Self {
        Self(rev)
    }

    pub fn into_rev(self) -> Vec<Rc<MalData>> {
        self.0
    }

    pub fn from_vec(mut vec: Vec<Rc<MalData>>) -> Self {
        vec.reverse();
        Self(vec)
    }

    pub fn into_vec(mut self) -> Vec<Rc<MalData>> {
        self.0.reverse();
        self.0
    }

    pub fn append(&mut self, other: &mut List) {
        other.0.append(&mut self.0);
        std::mem::swap(self, other);
    }
}

#[derive(Debug, Clone)]
pub struct Html {
    pub tag: String,
    pub properties: Vec<(String, Option<String>)>,
    pub children: Option<Vec<Rc<MalData>>>,
}

impl fmt::Display for Html {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{}", self.tag)?;
        for (key, value) in &self.properties {
            write!(f, " {key}")?;
            if let Some(value) = value {
                write!(f, "={value}")?;
            }
        }
        if let Some(children) = &self.children {
            write!(f, ">")?;
            for child in children {
                write!(f, "{}", child.value)?;
            }
            write!(f, "</{}>", self.tag)?;
        } else {
            write!(f, " />")?;
        }

        Ok(())
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum MalKey {
    Bool(bool),
    Int(i64),
    Kwd(String),
    Str(String),
    Sym(String),
}

impl MalKey {
    pub fn from_value(value: MalVal) -> Result<MalKey, MalVal> {
        match value {
            MalVal::Sym(value) => Ok(MalKey::Sym(value)),
            MalVal::Str(value) => Ok(MalKey::Str(value)),
            MalVal::Kwd(value) => Ok(MalKey::Kwd(value)),
            MalVal::Int(value) => Ok(MalKey::Int(value)),
            MalVal::Bool(value) => Ok(MalKey::Bool(value)),
            _ => Err(value),
        }
    }

    pub fn into_value(self) -> MalVal {
        match self {
            MalKey::Bool(value) => MalVal::Bool(value),
            MalKey::Int(value) => MalVal::Int(value),
            MalKey::Kwd(value) => MalVal::Kwd(value),
            MalKey::Str(value) => MalVal::Str(value),
            MalKey::Sym(value) => MalVal::Sym(value),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            MalKey::Bool(_) => MalVal::BOOL,
            MalKey::Int(_) => MalVal::INT,
            MalKey::Kwd(_) => MalVal::KWD,
            MalKey::Str(_) => MalVal::STR,
            MalKey::Sym(_) => MalVal::SYM,
        }
    }
}

pub struct CallContext {
    /// The current file that is executing the function.
    file: Option<Rc<String>>,
    dir: Option<DirContext>,
    /// The name of the function being called.
    frame: Option<(String, Location)>,
    frames: Vec<(String, Location)>,
    /// The environment returned from `(new-env)`
    default_env: Rc<Env>,
}

#[derive(Clone)]
pub struct DirContext {
    /// The directory to use when accessing relative file paths.
    dir: Rc<PathBuf>,
    /// The directory to use when accessing absolute file paths.
    root: Rc<PathBuf>,
}

impl DirContext {
    pub fn root(&self) -> &Rc<PathBuf> {
        &self.root
    }

    pub fn new(root: Rc<PathBuf>, file: &str) -> DirContext {
        let dir = root
            .join(file)
            .parent()
            .expect("File must have a parent")
            .to_path_buf();

        DirContext {
            dir: Rc::new(dir),
            root,
        }
    }

    pub fn canonicalize<P: AsRef<Path>>(&self, path: P) -> Option<PathBuf> {
        let path = path.as_ref();
        if path.is_absolute() {
            match path.strip_prefix("/") {
                Ok(path) => Some(self.root.join(path)),
                Err(_) => None,
            }
        } else {
            Some(self.dir.join(path))
        }
    }
}

impl CallContext {
    pub fn new_frame(&self, frame: (String, Location)) -> CallContext {
        CallContext {
            frame: Some(frame),
            frames: self.frames(),
            file: self.file.clone(),
            dir: self.dir.clone(),
            default_env: Rc::clone(&self.default_env),
        }
    }

    pub fn new_env(&self) -> Env {
        (*self.default_env).clone()
    }

    pub fn frames(&self) -> Vec<(String, Location)> {
        let mut frames = self.frames.clone();
        if let Some(frame) = self.frame.clone() {
            frames.push(frame);
        }
        frames
    }

    pub fn name(&self) -> Option<&str> {
        self.frame.as_ref().map(|(x, _)| x.as_str())
    }

    pub fn file(&self) -> Option<&str> {
        match &self.file {
            Some(file) => Some(file),
            None => None,
        }
    }

    pub fn dir_context(&self) -> Option<&DirContext> {
        self.dir.as_ref()
    }

    pub fn repl(dir: PathBuf) -> Self {
        let dir = Rc::new(dir);

        Self {
            frame: None,
            frames: vec![],
            file: None,
            dir: Some(DirContext {
                root: Rc::clone(&dir),
                dir,
            }),
            default_env: Rc::new(Env::std()),
        }
    }

    pub fn with_fs(default_env: Env, root: Rc<PathBuf>, file: impl Into<String>) -> CallContext {
        let file = file.into();

        Self {
            dir: Some(DirContext::new(root, &file)),
            file: Some(Rc::new(file)),
            frame: None,
            frames: vec![],
            default_env: Rc::new(default_env),
        }
    }

    pub fn inner(&self, file: impl Into<String>) -> Self {
        let file = file.into();

        Self {
            dir: self
                .dir
                .as_ref()
                .map(|ctx| DirContext::new(Rc::clone(&ctx.root), &file)),
            file: Some(Rc::new(file)),
            frame: None,
            frames: vec![],
            default_env: Rc::clone(&self.default_env),
        }
    }

    pub fn std() -> Self {
        Self {
            frame: Some(("std".into(), Location::new(Rc::new("std.mal".into()), 1, 1))),
            frames: vec![],
            file: None,
            dir: None,
            default_env: Rc::new(Env::empty("std".into())),
        }
    }
}
