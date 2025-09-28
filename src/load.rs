use std::{fmt, fs, io, path::PathBuf, rc::Rc};

use crate::{
    env::Env,
    reader::{self, Location},
    types::{CallContext, HtmlProperty, HtmlText, List, MalData, MalVal},
};

pub fn shtml(
    ctx: &CallContext,
    env: &mut Env,
    rel_path: &str,
    abs_path: PathBuf,
) -> Result<String, Error> {
    let input = match fs::File::open(&abs_path) {
        Ok(input) => input,
        Err(e) => return Err(Error::Io(e)),
    };

    let file_loc = Location::file(rel_path);

    match reader::parse_html_reader(file_loc.clone(), input) {
        Ok(ast) => {
            let ctx = ctx.inner(rel_path);
            let mut value = String::new();
            embed(&mut value, &env.eval(&ctx, ast).map_err(Error::SHtml)?)?;
            Ok(value)
        }
        Err(e) => Err(Error::Parse(e.to_string())),
    }
}

fn embed(f: &mut impl fmt::Write, value: &Rc<MalData>) -> Result<(), Error> {
    match &value.value {
        MalVal::Str(value) => write!(f, "{value}"),
        MalVal::Int(value) => write!(f, "{value}"),
        MalVal::Float(value) => write!(f, "{value}"),
        MalVal::Bool(value) => write!(f, "{value}"),
        MalVal::Nil => Ok(()),
        MalVal::Html(html) => {
            if html.has_doctype {
                writeln!(f, "<!DOCTYPE html>")?;
            }
            if let Some(tag) = &html.tag {
                write!(f, "<{tag}")?;
            }
            for prop in &html.properties {
                write!(f, " ")?;
                match prop {
                    HtmlProperty::Kvp(key, Some(value)) => {
                        write!(f, "{key}=")?;
                        embed(f, value)?;
                    }
                    HtmlProperty::Kvp(key, None) => write!(f, "{key}")?,
                    HtmlProperty::Key(key) => embed(f, key)?,
                }
            }
            if let Some(children) = &html.children {
                if html.tag.is_some() {
                    write!(f, ">")?;
                }
                for child in children {
                    match child {
                        HtmlText::Text(text) => write!(f, "{text}")?,
                        HtmlText::Value(value) => embed(f, value)?,
                    }
                }
                if let Some(tag) = &html.tag {
                    write!(f, "</{tag}>")?;
                }
            } else if html.tag.is_some() {
                write!(f, " />")?;
            }

            Ok(())
        }
        _ => return Err(Error::CannotEmbed(Rc::clone(value))),
    }
    .map_err(Error::Fmt)
}

pub fn mal(
    ctx: &CallContext,
    env: &mut Env,
    rel_path: &str,
    abs_path: PathBuf,
) -> Result<Rc<MalData>, Error> {
    let input = match fs::File::open(&abs_path) {
        Ok(input) => input,
        Err(e) => return Err(Error::Io(e)),
    };

    let file_loc = Location::file(rel_path);

    match reader::parse_mal_reader(file_loc.clone(), input) {
        Ok(vals) => {
            let mut vals = List::from_vec(vals);
            vals.push_front(MalVal::Sym("do".into()).with_loc(file_loc.clone()));

            let ctx = ctx.inner(rel_path);
            let ret = env
                .eval(&ctx, MalVal::List(vals).with_loc(file_loc))
                .map_err(Error::SHtml)?;

            Ok(ret)
        }
        Err(e) => Err(Error::Parse(e.to_string())),
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("load shtml: io: {0}")]
    Io(io::Error),
    #[error("load shtml: fmt: {0}")]
    Fmt(#[from] fmt::Error),
    #[error("load shtml: couldn't parse {0}")]
    Parse(String),
    #[error("load shtml: {0}")]
    SHtml(crate::Error),
    #[error("load shtml: cannot embed: '{}'", _0.type_name())]
    CannotEmbed(Rc<MalData>),
}
