use std::{collections::HashMap, fmt};

#[derive(Debug)]
pub enum MalVal {
    List(Vec<MalVal>),
    Vector(Vec<MalVal>),
    Map(HashMap<MalVal, MalVal>),
    Sym(String),
    Str(String),
    Kwd(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl fmt::Display for MalVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MalVal::List(vals) => join_vals(f, "(", ")", vals.into_iter()),
            MalVal::Vector(vals) => join_vals(f, "(", ")", vals.into_iter()),
            MalVal::Map(map) => {
                join_vals(f, "{", "}", map.into_iter().map(|(x, y)| [x, y]).flatten())
            }
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
