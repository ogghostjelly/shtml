use std::collections::HashMap;

use crate::types::{List, MalVal};

pub struct Env {
    data: HashMap<String, MalVal>,
    outer: Option<Box<Env>>,
}

impl Env {
    pub fn top(data: HashMap<String, MalVal>) -> Self {
        Self { data, outer: None }
    }

    pub fn eval(&mut self, ast: MalVal) -> Result<MalVal, Error> {
        match ast {
            MalVal::List(list) => {
                let list = List::from_inner(self.eval_in(list.into_inner())?);
                self.eval_list(list)
            }
            MalVal::Vector(vec) => self.eval_in(vec).map(MalVal::Vector),
            MalVal::Map(map) => {
                let mut ret = HashMap::with_capacity(map.len());
                for (key, value) in map.into_iter() {
                    ret.insert(key, self.eval(value)?);
                }
                Ok(MalVal::Map(ret))
            }
            MalVal::Sym(sym) => Ok(self
                .data
                .get(&sym)
                .ok_or_else(|| Error::NotFound(sym.to_string()))?
                .clone()),
            MalVal::Str(_)
            | MalVal::BuiltinFn(_)
            | MalVal::Kwd(_)
            | MalVal::Int(_)
            | MalVal::Float(_)
            | MalVal::Bool(_) => Ok(ast),
        }
    }

    fn eval_in(&mut self, vals: Vec<MalVal>) -> Result<Vec<MalVal>, Error> {
        let mut ret = Vec::with_capacity(vals.len());
        for value in vals {
            ret.push(self.eval(value)?);
        }
        Ok(ret)
    }

    fn eval_list(&mut self, mut vals: List) -> Result<MalVal, Error> {
        let Some(op) = vals.pop_front() else {
            return Ok(MalVal::List(vals));
        };

        match op {
            MalVal::BuiltinFn(f) => f(vals),
            MalVal::List(_)
            | MalVal::Vector(_)
            | MalVal::Map(_)
            | MalVal::Sym(_)
            | MalVal::Str(_)
            | MalVal::Kwd(_)
            | MalVal::Int(_)
            | MalVal::Float(_)
            | MalVal::Bool(_) => Err(Error::CannotApply(op.type_name())),
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("symbol not found '{0}'")]
    NotFound(String),
    #[error("cannot apply '{0}'")]
    CannotApply(&'static str),
    #[error("{0}")]
    Error(String),
}
