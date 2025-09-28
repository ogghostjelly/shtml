use std::{collections::HashMap, rc::Rc};

use indexmap::IndexMap;

use crate::{
    ns::{self, apply_map},
    reader::Location,
    types::{CallContext, List, MalData, MalFn, MalVal},
    Error, ErrorKind, MalRet,
};

#[derive(Clone, Debug)]
pub struct Env {
    name: String,
    data: HashMap<String, Rc<MalData>>,
    outer: Option<Rc<Env>>,
}

impl Env {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn empty(name: String) -> Self {
        Self {
            name,
            data: HashMap::new(),
            outer: None,
        }
    }

    pub fn std() -> Self {
        let mut env = Self {
            name: "std".into(),
            data: HashMap::new(),
            outer: None,
        };

        ns::std(&mut env);
        env
    }

    pub fn inner(name: String, env: &Env) -> Self {
        Self {
            name,
            data: HashMap::new(),
            outer: Some(Rc::new(env.clone())),
        }
    }

    pub fn set(&mut self, key: impl Into<String>, value: Rc<MalData>) {
        self.data.insert(key.into(), value);
    }

    pub fn set_fn(
        &mut self,
        loc: Location,
        key: impl Into<String>,
        value: fn(&CallContext, (List, Location)) -> MalRet,
    ) {
        let key = key.into();
        self.set(
            key.clone(),
            Rc::new(MalData {
                value: MalVal::BuiltinFn(key, value),
                loc,
            }),
        )
    }

    pub fn set_special(
        &mut self,
        loc: Location,
        key: impl Into<String>,
        value: fn(&CallContext, &mut Env, (List, Location)) -> TcoRet,
    ) {
        let key = key.into();
        self.set(
            key.clone(),
            Rc::new(MalData {
                value: MalVal::Special(key, value),
                loc,
            }),
        );
    }

    pub fn get(
        &self,
        ctx: &CallContext,
        loc: &Location,
        key: &String,
    ) -> Result<&Rc<MalData>, Error> {
        match self.data.get(key) {
            Some(value) => Ok(value),
            None => match &self.outer {
                Some(env) => env.get(ctx, loc, key),
                None => {
                    if key == "unquote" {
                        Err(Error::new(
                            ErrorKind::OutsideOfQuasiquote("unquote"),
                            ctx,
                            loc.clone(),
                        ))
                    } else if key == "unquote-splice" {
                        Err(Error::new(
                            ErrorKind::OutsideOfQuasiquote("unquote-splice"),
                            ctx,
                            loc.clone(),
                        ))
                    } else {
                        Err(Error::new(
                            ErrorKind::NotFound(key.to_string()),
                            ctx,
                            loc.clone(),
                        ))
                    }
                }
            },
        }
    }

    pub fn eval(&mut self, ctx: &CallContext, mut ast: Rc<MalData>) -> MalRet {
        loop {
            match &ast.value {
                MalVal::List(list) => match self.eval_list(ctx, (list, &ast.loc))? {
                    TcoVal::Val(val) => return Ok(val),
                    TcoVal::Unevaluated(val) => ast = val,
                },
                MalVal::Vector(vec) => {
                    return self
                        .eval_in(ctx, vec)
                        .map(|v| MalVal::Vector(v).with_loc(ast.loc.clone()))
                }
                MalVal::Map(map) => {
                    let mut ret = IndexMap::with_capacity(map.len());
                    for (key, value) in map.into_iter() {
                        ret.insert(key.clone(), self.eval(ctx, Rc::clone(value))?);
                    }
                    return Ok(MalVal::Map(ret).with_loc(ast.loc.clone()));
                }
                MalVal::Sym(sym) => return self.get(ctx, &ast.loc, sym).cloned(),
                MalVal::Str(_)
                | MalVal::Nil
                | MalVal::Env(_)
                | MalVal::BuiltinFn(_, _)
                | MalVal::Fn { .. }
                | MalVal::Special(_, _)
                | MalVal::Kwd(_)
                | MalVal::Int(_)
                | MalVal::Float(_)
                | MalVal::Html(_)
                | MalVal::Bool(_) => return Ok(ast),
            }
        }
    }

    pub fn eval_tco(&mut self, source: &CallContext, tco: TcoVal) -> MalRet {
        match tco {
            TcoVal::Val(val) => Ok(val),
            TcoVal::Unevaluated(val) => self.eval(source, val),
        }
    }

    fn eval_in(
        &mut self,
        ctx: &CallContext,
        vals: &Vec<Rc<MalData>>,
    ) -> Result<Vec<Rc<MalData>>, Error> {
        let mut ret = Vec::with_capacity(vals.len());
        for value in vals {
            ret.push(self.eval(ctx, Rc::clone(value))?);
        }
        Ok(ret)
    }

    fn eval_list(&mut self, ctx: &CallContext, (vals, loc): (&List, &Location)) -> TcoRet {
        let mut vals = vals.clone();

        let Some(op) = vals.pop_front() else {
            return Ok(TcoVal::Val(
                MalVal::List(vals.clone()).with_loc(loc.clone()),
            ));
        };

        let op = self.eval(ctx, op)?;

        match &op.value {
            MalVal::BuiltinFn(name, f) => f(
                &ctx.new_frame((name.to_string(), loc.clone())),
                (
                    List::from_rev(self.eval_in(ctx, &vals.clone().into_rev())?),
                    loc.clone(),
                ),
            )
            .map(TcoVal::Val),
            MalVal::Fn(MalFn {
                name,
                is_macro,
                outer,
                binds,
                bind_rest,
                body,
            }) => {
                let name = name.clone().unwrap_or_else(|| "lambda".to_string());
                let ctx = &ctx.new_frame((name.clone(), loc.clone()));

                let bind_rest = match &bind_rest {
                    Some(bind) => bind.as_ref(),
                    None => {
                        if binds.len() != vals.len() {
                            return Err(Error::new(
                                ErrorKind::ArityMismatch(binds.len(), vals.len()),
                                ctx,
                                loc.clone(),
                            ));
                        }
                        None
                    }
                };

                let mut env = Env::inner(name, outer);
                let bindings = binds.iter();
                let mut vals = vals.iter().cloned();

                for (key, value) in bindings.zip(&mut vals) {
                    let value = if *is_macro {
                        value
                    } else {
                        self.eval(ctx, value)?
                    };
                    env.set(key, value)
                }

                {
                    let mut ls = vec![];

                    for value in vals {
                        let value = if *is_macro {
                            value
                        } else {
                            self.eval(ctx, value)?
                        };
                        ls.push(value)
                    }

                    if let Some((loc, key)) = bind_rest.cloned() {
                        env.set(key, MalVal::List(List::from_vec(ls)).with_loc(loc))
                    }
                }

                let mut last = Rc::clone(&body.0);

                for value in body.1.iter().cloned() {
                    env.eval(ctx, last)?;
                    last = value;
                }

                Ok(if *is_macro {
                    TcoVal::Unevaluated(env.eval(ctx, last)?)
                } else {
                    // Returning Val instead of Unevaluated might make TCO completely useless
                    // but it needs to happen since `last` has to be evaluated in the `env` environment
                    // I have no idea if this is a bad idea or not ;-;
                    TcoVal::Val(env.eval(ctx, last)?)
                })
            }
            MalVal::Map(map) => apply_map(
                map,
                &ctx.new_frame(("map/get".to_string(), loc.clone())),
                (
                    List::from_rev(self.eval_in(ctx, &vals.clone().into_rev())?),
                    loc.clone(),
                ),
            )
            .map(TcoVal::Val),
            MalVal::Special(_, f) => f(ctx, self, (vals.clone(), loc.clone())),
            MalVal::List(_)
            | MalVal::Nil
            | MalVal::Env(_)
            | MalVal::Vector(_)
            | MalVal::Sym(_)
            | MalVal::Str(_)
            | MalVal::Kwd(_)
            | MalVal::Int(_)
            | MalVal::Html(_)
            | MalVal::Float(_)
            | MalVal::Bool(_) => Err(Error::new(
                ErrorKind::CannotApply(op.type_name()),
                ctx,
                loc.clone(),
            )),
        }
    }
}

pub type TcoRet = Result<TcoVal, Error>;

pub enum TcoVal {
    Val(Rc<MalData>),
    Unevaluated(Rc<MalData>),
}
