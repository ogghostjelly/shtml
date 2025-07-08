use std::collections::HashMap;

use crate::types::{List, MalRet, MalVal, TcoRet};

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

    pub fn set(&mut self, key: impl Into<String>, value: MalVal) {
        self.data.insert(key.into(), value);
    }

    pub fn set_fn(&mut self, key: impl Into<String>, value: fn(List) -> MalRet) {
        let key = key.into();
        self.set(key.clone(), MalVal::BuiltinFn(key, value))
    }

    pub fn set_special(&mut self, key: impl Into<String>, value: fn(&mut Env, List) -> TcoRet) {
        let key = key.into();
        self.set(key.clone(), MalVal::Special(key, value));
    }

    pub fn get(&self, key: String) -> Result<&MalVal, Error> {
        match self.data.get(&key) {
            Some(value) => Ok(value),
            None => match &self.outer {
                Some(env) => env.get(key),
                None => {
                    if key == "unquote" {
                        Err(Error::OutsideOfQuasiquote("unquote"))
                    } else if key == "unquote-splice" {
                        Err(Error::OutsideOfQuasiquote("unquote-splice"))
                    } else {
                        Err(Error::NotFound(key))
                    }
                }
            },
        }
    }

    pub fn eval(&mut self, mut ast: MalVal) -> MalRet {
        loop {
            match ast {
                MalVal::List(list) => match self.eval_list(list)? {
                    TcoVal::Val(val) => return Ok(val),
                    TcoVal::Unevaluated(val) => ast = val,
                },
                MalVal::Vector(vec) => return self.eval_in(vec).map(MalVal::Vector),
                MalVal::Map(map) => {
                    let mut ret = HashMap::with_capacity(map.len());
                    for (key, value) in map.into_iter() {
                        ret.insert(key, self.eval(value)?);
                    }
                    return Ok(MalVal::Map(ret));
                }
                MalVal::Sym(sym) => return self.get(sym).cloned(),
                MalVal::Str(_)
                | MalVal::BuiltinFn(_, _)
                | MalVal::Fn { .. }
                | MalVal::Special(_, _)
                | MalVal::Kwd(_)
                | MalVal::Int(_)
                | MalVal::Float(_)
                | MalVal::Bool(_) => return Ok(ast),
            }
        }
    }

    pub fn eval_tco(&mut self, tco: TcoVal) -> MalRet {
        match tco {
            TcoVal::Val(val) => Ok(val),
            TcoVal::Unevaluated(val) => self.eval(val),
        }
    }

    fn eval_in(&mut self, vals: Vec<MalVal>) -> Result<Vec<MalVal>, Error> {
        let mut ret = Vec::with_capacity(vals.len());
        for value in vals {
            ret.push(self.eval(value)?);
        }
        Ok(ret)
    }

    fn eval_list(&mut self, mut vals: List) -> TcoRet {
        let Some(op) = vals.pop_front() else {
            return Ok(TcoVal::Val(MalVal::List(vals)));
        };

        let op = self.eval(op)?;

        match op {
            MalVal::BuiltinFn(_, f) => {
                f(List::from_rev(self.eval_in(vals.into_rev())?)).map(TcoVal::Val)
            }
            MalVal::Fn {
                name: _,
                outer,
                binds,
                bind_rest,
                body,
            } => {
                let bind_rest = match &bind_rest {
                    Some(bind) => bind.as_ref(),
                    None => {
                        if binds.len() != vals.len() {
                            return Err(Error::ArityMismatch(binds.len(), vals.len()));
                        }
                        None
                    }
                };

                let mut env = Env::inner(&outer);
                let bindings = binds.into_iter();
                let mut vals = vals.into_iter();

                for (key, value) in bindings.zip(&mut vals) {
                    let value = self.eval(value)?;
                    env.set(key, value)
                }

                {
                    let mut ls = vec![];

                    for value in vals {
                        let value = self.eval(value)?;
                        ls.push(value)
                    }

                    if let Some(key) = bind_rest {
                        env.set(key.clone(), MalVal::List(List::from_vec(ls)))
                    }
                }

                let mut last = *body.0;

                for value in body.1.into_iter() {
                    env.eval(last)?;
                    last = value;
                }

                Ok(TcoVal::Unevaluated(last))
            }
            MalVal::Special(_, f) => f(self, vals),
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

pub enum TcoVal {
    Val(MalVal),
    Unevaluated(MalVal),
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
    #[error("cannot have more binds after variadic '&'")]
    BindsAfterRest,
    #[error("symbol not found '{0}' cannot be used outside of quasiquote")]
    OutsideOfQuasiquote(&'static str),
}
