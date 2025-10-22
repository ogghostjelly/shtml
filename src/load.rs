use std::{fmt, fs, io, path::PathBuf};

use crate::{
    env::Env,
    reader::{self, Location},
    types::{CallContext, HtmlText, List, MalData, MalVal},
};

pub fn shtml(
    ctx: &CallContext,
    env: &mut Env,
    rel_path: &str,
    abs_path: PathBuf,
) -> Result<String, Error> {
    let input = match fs::File::open(&abs_path) {
        Ok(input) => input,
        Err(e) => return Err(Error::Io(rel_path.to_string(), e)),
    };

    let file_loc = Location::file(rel_path);

    match reader::parse_html_reader(file_loc.clone(), input) {
        Ok(ast) => {
            let ctx = ctx.inner(rel_path);
            let mut value = String::new();
            embed(
                &mut value,
                &env.eval(&ctx, ast)
                    .map_err(|e| Error::Shtml(rel_path.to_string(), Box::new(e)))?,
            )?;
            Ok(value)
        }
        Err(e) => Err(Error::Parse(e.to_string())),
    }
}

pub fn embed(f: &mut impl fmt::Write, value: &MalData) -> Result<(), Error> {
    match value.value.as_ref() {
        MalVal::List(ls) => {
            for value in ls.iter() {
                embed(f, value)?;
            }
            Ok(())
        }
        MalVal::Vector(ls) => {
            for value in ls {
                embed(f, value)?;
            }
            Ok(())
        }
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
                for key in &prop.key {
                    match key {
                        HtmlText::Text(text) => write!(f, "{text}")?,
                        HtmlText::Value(value) => embed(f, value)?,
                    }
                }
                if let Some(value) = &prop.value {
                    write!(f, "=\"")?;
                    for key in value {
                        match key {
                            HtmlText::Text(text) => write!(f, "{text}")?,
                            HtmlText::Value(value) => embed(f, value)?,
                        }
                    }
                    write!(f, "\"")?;
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
        _ => return Err(Error::CannotEmbed(value.clone())),
    }
    .map_err(Error::Fmt)
}

pub fn mal(
    ctx: &CallContext,
    env: &mut Env,
    rel_path: &str,
    abs_path: PathBuf,
) -> Result<MalData, Error> {
    let input = match fs::File::open(&abs_path) {
        Ok(input) => input,
        Err(e) => return Err(Error::Io(rel_path.to_string(), e)),
    };

    let file_loc = Location::file(rel_path);

    match reader::parse_mal_reader(file_loc.clone(), input) {
        Ok(vals) => {
            let mut vals = List::from_vec(vals);
            vals.push_front(MalVal::Sym("do".into()).with_loc(file_loc.clone()));

            let ctx = ctx.inner(rel_path);
            let ret = env
                .eval(&ctx, MalVal::List(vals).with_loc(file_loc))
                .map_err(|e| Error::Shtml(rel_path.to_string(), Box::new(e)))?;

            Ok(ret)
        }
        Err(e) => Err(Error::Parse(e.to_string())),
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("load shtml in {0:?}: io: {1}")]
    Io(String, io::Error),
    #[error("load shtml: fmt: {0}")]
    Fmt(#[from] fmt::Error),
    #[error("load shtml: couldn't parse {0}")]
    Parse(String),
    #[error("load shtml in {0:?}: {1}")]
    Shtml(String, Box<crate::Error>),
    #[error("load shtml: cannot embed: '{}': {}", _0.type_name(), _0.value)]
    CannotEmbed(MalData),
}
