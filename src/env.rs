use std::collections::HashMap;

use crate::types::{List, MalVal};

#[derive(Clone, Debug)]
pub struct Env {
    data: HashMap<String, MalVal>,
    outer: Option<Box<Env>>,
}

impl Env {
    pub fn new(data: HashMap<String, MalVal>, outer: Option<Box<Env>>) -> Self {
        Self { data, outer }
    }

    pub fn top(data: HashMap<String, MalVal>) -> Self {
        Self::new(data, None)
    }

    pub fn inner(env: &Env) -> Self {
        Self::new(HashMap::new(), Some(Box::new(env.clone())))
    }

    pub fn set(&mut self, key: String, value: MalVal) {
        self.data.insert(key, value);
    }

    pub fn get(&self, key: String) -> Result<&MalVal, Error> {
        match self.data.get(&key) {
            Some(value) => Ok(value),
            None => match &self.outer {
                Some(env) => env.get(key),
                None => Err(Error::NotFound(key)),
            },
        }
    }

    pub fn eval(&mut self, ast: MalVal) -> Result<MalVal, Error> {
        match ast {
            MalVal::List(list) => self.eval_list(list),
            MalVal::Vector(vec) => self.eval_in(vec).map(MalVal::Vector),
            MalVal::Map(map) => {
                let mut ret = HashMap::with_capacity(map.len());
                for (key, value) in map.into_iter() {
                    ret.insert(key, self.eval(value)?);
                }
                Ok(MalVal::Map(ret))
            }
            MalVal::Sym(sym) => self.get(sym).cloned(),
            MalVal::Str(_)
            | MalVal::BuiltinFn(_)
            | MalVal::Fn { .. }
            | MalVal::BuiltinMacro(_)
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

        let op = self.eval(op)?;

        match op {
            MalVal::BuiltinFn(f) => f(List::from_inner(self.eval_in(vals.into_inner())?)),
            MalVal::Fn {
                outer,
                bindings,
                body,
            } => {
                if bindings.len() != vals.len() {
                    return Err(Error::ArityMismatch(bindings.len(), vals.len()));
                }

                let mut env = Env::inner(&outer);
                let bindings = bindings.into_iter();
                let vals = vals.into_iter();

                for (key, value) in bindings.zip(vals) {
                    let value = self.eval(value)?;
                    env.set(key, value)
                }

                let mut last = env.eval(*body.0)?;
                for value in body.1.into_iter() {
                    last = self.eval(value)?;
                }
                Ok(last)
            }
            MalVal::BuiltinMacro(f) => f(self, vals),
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
    #[error("cannot use '{op}' on '{fst}' and '{snd}'")]
    InvalidOperation {
        op: &'static str,
        fst: &'static str,
        snd: &'static str,
    },
    #[error("cannot use '{0}' on '{1}'")]
    InvalidOperation1(&'static str, &'static str),
    #[error("expected type '{0}' but got '{1}'")]
    UnexpectedType(&'static str, &'static str),
    #[error("'{0}' expects an even number of arguments")]
    UnevenArguments(&'static str),
    #[error("'{0}' cannot be a map key")]
    InvalidMapKey(&'static str),
    #[error("expected {0} arguments but got {1}")]
    ArityMismatch(usize, usize),
    #[error("expected at least {0} arguments but got {1}")]
    AtleastArityMismatch(usize, usize),
}
