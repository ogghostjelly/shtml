use std::{fs, io, path::PathBuf, rc::Rc};

use crate::{
    env::Env,
    reader::{self, Location},
    types::{CallContext, List, MalData, MalVal},
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

    match reader::parse_reader(file_loc.clone(), input) {
        Ok(els) => {
            let ctx = ctx.inner(rel_path);
            let mut value = String::new();

            for ast in els {
                let data = env.eval(&ctx, ast).map_err(Error::SHtml)?;

                let text = match &data.value {
                    MalVal::Nil => "".to_string(),
                    MalVal::Int(val) => val.to_string(),
                    MalVal::Float(val) => val.to_string(),
                    MalVal::Bool(val) => val.to_string(),
                    MalVal::Str(val) => val.to_string(),
                    _ => return Err(Error::CannotEmbed(data)),
                };

                value.push_str(&text);
            }

            Ok(value)
        }
        Err(e) => Err(Error::Parse(e.to_string())),
    }
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

    match reader::parse_reader(file_loc.clone(), input) {
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
    #[error("load shtml: couldn't parse {0}")]
    Parse(String),
    #[error("load shtml: {0}")]
    SHtml(crate::Error),
    #[error("load shtml: cannot embed: '{}'", _0.type_name())]
    CannotEmbed(Rc<MalData>),
}
