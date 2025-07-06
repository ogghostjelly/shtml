use std::{collections::HashMap, fmt};

#[derive(Debug, Clone)]
pub enum MalVal {
    List(Vec<MalVal>),
    Vector(Vec<MalVal>),
    Map(HashMap<MalKey, MalVal>),
    Sym(String),
    Str(String),
    Kwd(String),
    Int(i64),
    Float(f64),
    Bool(bool),
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
    fn from_value(value: MalVal) -> Option<MalKey> {
        match value {
            MalVal::List(_) | MalVal::Vector(_) | MalVal::Map(_) | MalVal::Float(_) => None,
            MalVal::Sym(value) => Some(MalKey::Sym(value)),
            MalVal::Str(value) => Some(MalKey::Str(value)),
            MalVal::Kwd(value) => Some(MalKey::Kwd(value)),
            MalVal::Int(value) => Some(MalKey::Int(value)),
            MalVal::Bool(value) => Some(MalKey::Bool(value)),
        }
    }

    fn into_value(self) -> MalVal {
        match self {
            MalKey::Bool(value) => MalVal::Bool(value),
            MalKey::Int(value) => MalVal::Int(value),
            MalKey::Kwd(value) => MalVal::Kwd(value),
            MalKey::Str(value) => MalVal::Str(value),
            MalKey::Sym(value) => MalVal::Sym(value),
        }
    }
}

impl fmt::Display for MalVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MalVal::List(vals) => join_vals(f, "(", ")", vals.into_iter()),
            MalVal::Vector(vals) => join_vals(f, "(", ")", vals.into_iter()),
            MalVal::Map(map) => join_vals(
                f,
                "{",
                "}",
                map.into_iter()
                    .map(|(key, value)| (key.clone().into_value(), value))
                    .collect::<Vec<_>>()
                    .iter()
                    .map(|(x, y)| [x, y])
                    .flatten(),
            ),
            MalVal::Sym(value) => write!(f, "{value}"),
            MalVal::Str(value) => write!(f, "\"{}\"", escape(value)),
            MalVal::Kwd(value) => write!(f, ":{value}"),
            MalVal::Int(value) => write!(f, "{value}"),
            MalVal::Float(value) => write!(f, "{value}."),
            MalVal::Bool(value) => write!(f, "{value}"),
        }
    }
}

fn join_vals<'a>(
    f: &mut fmt::Formatter<'_>,
    start: &str,
    end: &str,
    mut vals: impl Iterator<Item = &'a MalVal>,
) -> fmt::Result {
    write!(f, "{start}")?;
    if let Some(val) = vals.next() {
        write!(f, "{val}")?;
    }
    for val in vals {
        write!(f, " {val}")?;
    }
    write!(f, "{end}")
}

fn escape(input: &str) -> String {
    input
        .replace("\r", "\\r")
        .replace("\n", "\\n")
        .replace("\t", "\\t")
        .replace("\"", "\\\"")
        .replace("\\", "\\\\")
}
