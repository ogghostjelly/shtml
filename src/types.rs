use std::{collections::HashMap, iter, slice, vec};

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
    Vector(Vec<MalData>),
    Map(HashMap<MalKey, MalData>),
    Sym(String),
    Str(String),
    Kwd(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    BuiltinFn(String, fn(Location, List) -> MalRet),
    Special(String, fn(&mut Env, Location, List) -> TcoRet),
    Fn(MalFn),
}

impl MalData {
    pub fn is_true(&self) -> bool {
        match &self.value {
            MalVal::List(list) => !list.is_empty(),
            MalVal::Bool(value) => *value,
            _ => true,
        }
    }

    pub fn type_name(&self) -> &'static str {
        self.value.type_name()
    }
}

impl MalVal {
    pub fn with_loc(self, loc: Location) -> MalData {
        MalData { value: self, loc }
    }

    pub const LIST: &str = "list";
    pub const VECTOR: &str = "vector";
    pub const LIST_LIKE: &str = "list-like";
    pub const MAP: &str = "map";
    pub const SYM: &str = "symbol";
    pub const STR: &str = "string";
    pub const KWD: &str = "keyword";
    pub const INT: &str = "int";
    pub const FLOAT: &str = "float";
    pub const BOOL: &str = "bool";
    pub const FN: &str = "function";
    pub const MACRO: &str = "macro";

    pub fn type_name(&self) -> &'static str {
        match self {
            MalVal::List(_) => Self::LIST,
            MalVal::Vector(_) => Self::VECTOR,
            MalVal::Map(_) => Self::MAP,
            MalVal::Sym(_) => Self::SYM,
            MalVal::Str(_) => Self::STR,
            MalVal::Kwd(_) => Self::KWD,
            MalVal::Int(_) => Self::INT,
            MalVal::Float(_) => Self::FLOAT,
            MalVal::Bool(_) => Self::BOOL,
            MalVal::BuiltinFn(_, _) | MalVal::Fn { .. } => Self::FN,
            MalVal::Special(_, _) => Self::MACRO,
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
    pub body: (Box<MalData>, List),
}

#[derive(Debug, Clone)]
pub struct List(Vec<MalData>);

impl Default for List {
    fn default() -> Self {
        Self::new()
    }
}

impl List {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn first(&self) -> Option<&MalData> {
        self.0.last()
    }

    pub fn swap_remove(&mut self, index: usize) -> MalData {
        self.0.swap_remove(self.0.len() - index - 1)
    }

    pub fn split_off(&mut self, at: usize) -> Self {
        let mut o = self.0.split_off(self.len() - at);
        std::mem::swap(&mut self.0, &mut o);
        Self(o)
    }

    pub fn into_array<const N: usize>(self) -> Result<[MalData; N], List> {
        self.0
            .try_into()
            .map_err(List)
            .map(|mut arr: [MalData; N]| {
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

    pub fn iter(&self) -> iter::Rev<slice::Iter<MalData>> {
        self.0.iter().rev()
    }

    pub fn pop_front(&mut self) -> Option<MalData> {
        self.0.pop()
    }

    pub fn push_front(&mut self, value: MalData) {
        self.0.push(value);
    }
}

impl IntoIterator for List {
    type Item = MalData;

    type IntoIter = iter::Rev<vec::IntoIter<MalData>>;

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
    pub fn from_rev(rev: Vec<MalData>) -> Self {
        Self(rev)
    }

    pub fn into_rev(self) -> Vec<MalData> {
        self.0
    }

    pub fn from_vec(mut vec: Vec<MalData>) -> Self {
        vec.reverse();
        Self(vec)
    }

    pub fn into_vec(mut self) -> Vec<MalData> {
        self.0.reverse();
        self.0
    }

    pub fn append(&mut self, other: &mut List) {
        other.0.append(&mut self.0);
        std::mem::swap(self, other);
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
}
