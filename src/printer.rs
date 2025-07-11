use std::fmt;

use crate::types::{MalFn, MalVal};

impl fmt::Display for MalVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display(self, f, false)
    }
}

fn display(value: &MalVal, f: &mut fmt::Formatter<'_>, mut quote: bool) -> fmt::Result {
    match value {
        MalVal::List(vals) => {
            if !quote {
                write!(f, "'")?;
                quote = true;
            };
            join_vals(f, "(", ")", vals.iter().map(|x| &x.value), quote)
        }
        MalVal::Vector(vals) => join_vals(f, "[", "]", vals.iter().map(|x| &x.value), quote),
        MalVal::Map(map) => join_vals(
            f,
            "{",
            "}",
            map.iter()
                .map(|(key, value)| (key.clone().into_value(), &value.value))
                .collect::<Vec<_>>()
                .iter()
                .flat_map(|(x, y)| [x, y]),
            quote,
        ),
        MalVal::Sym(value) => {
            if !quote {
                write!(f, "'")?;
            };
            write!(f, "{value}")
        }
        MalVal::Str(value) => write!(f, "\"{}\"", escape(value)),
        MalVal::Kwd(value) => write!(f, ":{value}"),
        MalVal::Int(value) => write!(f, "{value}"),
        MalVal::Float(value) => write!(f, "{value:?}"),
        MalVal::Bool(value) => write!(f, "{value}"),
        MalVal::BuiltinFn(name, _)
        | MalVal::Fn(MalFn {
            name: Some(name),
            is_macro: false,
            ..
        }) => write!(f, "#<function:{name}>"),
        MalVal::Fn(MalFn {
            name: None,
            is_macro: false,
            ..
        }) => write!(f, "#<function>"),
        MalVal::Special(name, _)
        | MalVal::Fn(MalFn {
            name: Some(name),
            is_macro: true,
            ..
        }) => write!(f, "#<macro:{name}>"),
        MalVal::Fn(MalFn {
            name: None,
            is_macro: true,
            ..
        }) => write!(f, "#<macro>"),
        MalVal::Env(env) => write!(f, "#<env:{}>", env.name()),
    }
}

fn join_vals<'a>(
    f: &mut fmt::Formatter<'_>,
    start: &str,
    end: &str,
    mut vals: impl Iterator<Item = &'a MalVal>,
    quote: bool,
) -> fmt::Result {
    write!(f, "{start}")?;
    if let Some(val) = vals.next() {
        display(val, f, quote)?;
    }
    for val in vals {
        write!(f, " ")?;
        display(val, f, quote)?;
    }
    write!(f, "{end}")
}

fn escape(input: &str) -> String {
    input
        .replace("\r", r#"\r"#)
        .replace("\n", r#"\n"#)
        .replace("\t", r#"\t"#)
        .replace("\"", r#"\""#)
        .replace("\\", r#"\\"#)
}
