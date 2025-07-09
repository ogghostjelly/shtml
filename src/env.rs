use std::collections::HashMap;

use crate::{
    reader::Location,
    types::{List, MalData, MalFn, MalVal},
    Error, ErrorKind, MalRet,
};

#[derive(Clone, Debug)]
pub struct Env {
    data: HashMap<String, MalData>,
    outer: Option<Box<Env>>,
}

impl Env {
    pub fn new(data: HashMap<String, MalData>, outer: Option<Box<Env>>) -> Self {
        Self { data, outer }
    }

    pub fn empty() -> Self {
        Self::new(HashMap::new(), None)
    }

    pub fn inner(env: &Env) -> Self {
        Self::new(HashMap::new(), Some(Box::new(env.clone())))
    }

    pub fn set(&mut self, key: impl Into<String>, value: MalData) {
        self.data.insert(key.into(), value);
    }

    pub fn set_fn(
        &mut self,
        loc: Location,
        key: impl Into<String>,
        value: fn(Location, List) -> MalRet,
    ) {
        let key = key.into();
        self.set(
            key.clone(),
            MalData {
                value: MalVal::BuiltinFn(key, value),
                loc,
            },
        )
    }

    pub fn set_special(
        &mut self,
        loc: Location,
        key: impl Into<String>,
        value: fn(&mut Env, Location, List) -> TcoRet,
    ) {
        let key = key.into();
        self.set(
            key.clone(),
            MalData {
                value: MalVal::Special(key, value),
                loc,
            },
        );
    }

    pub fn get(&self, loc: Location, key: String) -> Result<&MalData, Error> {
        match self.data.get(&key) {
            Some(value) => Ok(value),
            None => match &self.outer {
                Some(env) => env.get(loc, key),
                None => {
                    if key == "unquote" {
                        Err(Error::new(loc, ErrorKind::OutsideOfQuasiquote("unquote")))
                    } else if key == "unquote-splice" {
                        Err(Error::new(
                            loc,
                            ErrorKind::OutsideOfQuasiquote("unquote-splice"),
                        ))
                    } else {
                        Err(Error::new(loc, ErrorKind::NotFound(key)))
                    }
                }
            },
        }
    }

    pub fn eval(&mut self, mut ast: MalData) -> MalRet {
        loop {
            match ast.value {
                MalVal::List(list) => match self.eval_list(ast.loc, list)? {
                    TcoVal::Val(val) => return Ok(val),
                    TcoVal::Unevaluated(val) => ast = val,
                },
                MalVal::Vector(vec) => {
                    return self
                        .eval_in(vec)
                        .map(|v| MalVal::Vector(v).with_loc(ast.loc))
                }
                MalVal::Map(map) => {
                    let mut ret = HashMap::with_capacity(map.len());
                    for (key, value) in map.into_iter() {
                        ret.insert(key, self.eval(value)?);
                    }
                    return Ok(MalVal::Map(ret).with_loc(ast.loc));
                }
                MalVal::Sym(sym) => return self.get(ast.loc, sym).cloned(),
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

    fn eval_in(&mut self, vals: Vec<MalData>) -> Result<Vec<MalData>, Error> {
        let mut ret = Vec::with_capacity(vals.len());
        for value in vals {
            ret.push(self.eval(value)?);
        }
        Ok(ret)
    }

    fn eval_list(&mut self, loc: Location, mut vals: List) -> TcoRet {
        let Some(op) = vals.pop_front() else {
            return Ok(TcoVal::Val(MalVal::List(vals).with_loc(loc)));
        };

        let op = self.eval(op)?;

        match op.value {
            MalVal::BuiltinFn(_, f) => {
                f(loc, List::from_rev(self.eval_in(vals.into_rev())?)).map(TcoVal::Val)
            }
            MalVal::Fn(MalFn {
                name,
                is_macro,
                outer,
                binds,
                bind_rest,
                body,
            }) => {
                let bind_rest = match &bind_rest {
                    Some(bind) => bind.as_ref(),
                    None => {
                        if binds.len() != vals.len() {
                            return Err(Error::new(
                                loc,
                                ErrorKind::ArityMismatch(
                                    binds.len(),
                                    vals.len(),
                                    name.unwrap_or_else(|| "lambda".to_string()),
                                ),
                            ));
                        }
                        None
                    }
                };

                let mut env = Env::inner(&outer);
                let bindings = binds.into_iter();
                let mut vals = vals.into_iter();

                for (key, value) in bindings.zip(&mut vals) {
                    let value = if is_macro { value } else { self.eval(value)? };
                    env.set(key, value)
                }

                {
                    let mut ls = vec![];

                    for value in vals {
                        let value = if is_macro { value } else { self.eval(value)? };
                        ls.push(value)
                    }

                    if let Some((loc, key)) = bind_rest.cloned() {
                        env.set(key, MalVal::List(List::from_vec(ls)).with_loc(loc))
                    }
                }

                let mut last = *body.0;

                for value in body.1.into_iter() {
                    env.eval(last)?;
                    last = value;
                }

                Ok(if is_macro {
                    TcoVal::Unevaluated(env.eval(last)?)
                } else {
                    // Returning Val instead of Unevaluated might make TCO completely useless
                    // but it needs to happen since `last` has to be evaluated in the `env` environment
                    // I have no idea if this is a bad idea or not ;-;
                    TcoVal::Val(env.eval(last)?)
                })
            }
            MalVal::Special(_, f) => f(self, loc, vals),
            MalVal::List(_)
            | MalVal::Vector(_)
            | MalVal::Map(_)
            | MalVal::Sym(_)
            | MalVal::Str(_)
            | MalVal::Kwd(_)
            | MalVal::Int(_)
            | MalVal::Float(_)
            | MalVal::Bool(_) => Err(Error::new(loc, ErrorKind::CannotApply(op.type_name()))),
        }
    }
}

pub type TcoRet = Result<TcoVal, Error>;

pub enum TcoVal {
    Val(MalData),
    Unevaluated(MalData),
}
