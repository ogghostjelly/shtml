use std::{fs, io, path::PathBuf, rc::Rc};

use crate::{
    env::Env,
    reader::{self, Element, Location},
    types::{CallContext, List, MalData, MalVal},
};

pub fn shtml(
    env: &mut Env,
    root: Rc<PathBuf>,
    rel_path: &str,
    abs_path: PathBuf,
) -> Result<String, Error> {
    let input = match fs::read_to_string(&abs_path) {
        Ok(input) => input,
        Err(e) => return Err(Error::Io(e)),
    };

    let file_loc = Location::file(rel_path);

    match reader::parse_file(file_loc.clone(), &input) {
        Ok(els) => {
            let ctx = CallContext::new(root, abs_path);
            let mut value = String::new();

            for el in els {
                let text = match el {
                    Element::Text(val) => val,
                    Element::Value(ast) => {
                        let data = env.eval(&ctx, ast).map_err(Error::SHtml)?;

                        match data.value {
                            MalVal::List(list) if list.is_empty() => "".to_string(),
                            MalVal::Int(val) => val.to_string(),
                            MalVal::Float(val) => val.to_string(),
                            MalVal::Bool(val) => val.to_string(),
                            MalVal::Str(val) => val,
                            _ => return Err(Error::CannotEmbed(data)),
                        }
                    }
                };

                value.push_str(&text);
            }

            Ok(value)
        }
        Err(e) => Err(Error::Parse(e.to_string())),
    }
}

pub fn mal(
    env: &mut Env,
    root: Rc<PathBuf>,
    rel_path: &str,
    abs_path: PathBuf,
) -> Result<MalData, Error> {
    let input = match fs::read_to_string(&abs_path) {
        Ok(input) => input,
        Err(e) => return Err(Error::Io(e)),
    };

    let file_loc = Location::file(rel_path);

    match reader::parse(file_loc.clone(), &input) {
        Ok(vals) => {
            let mut vals = List::from_vec(vals);
            vals.push_front(MalVal::Sym("do".into()).with_loc(file_loc.clone()));

            let ctx = CallContext::new(root, abs_path);
            let ret = env
                .eval(&ctx, MalVal::List(vals).with_loc(file_loc))
                .map_err(Error::SHtml)?;

            Ok(ret)
        }
        Err(e) => Err(Error::Parse(e.to_string())),
    }
}

pub enum Error {
    Io(io::Error),
    Parse(String),
    SHtml(crate::Error),
    CannotEmbed(MalData),
}
