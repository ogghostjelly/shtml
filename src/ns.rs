use std::collections::HashMap;

use crate::{
    env::Error,
    types::{List, MalVal},
};

pub fn std(data: &mut HashMap<String, MalVal>) {
    sform(data);
    math(data);
    ds(data);
}

pub fn sform(data: &mut HashMap<String, MalVal>) {
    data.insert("let*".to_string(), MalVal::BuiltinMacro(sform::r#let));
    data.insert("def!".to_string(), MalVal::BuiltinMacro(sform::def));
    data.insert("do".to_string(), MalVal::BuiltinMacro(sform::r#do));
    data.insert("if".to_string(), MalVal::BuiltinMacro(sform::r#if));
    data.insert("fn*".to_string(), MalVal::BuiltinMacro(sform::r#fn));
}

mod sform {
    use crate::{
        env::{Env, Error},
        ns::{take_atleast, to_iter},
        types::{List, MalVal},
    };

    use super::{take_exact, to_sym};

    pub fn r#let(env: &mut Env, args: List) -> Result<MalVal, Error> {
        let [bindings, body] = take_exact(args)?;
        let mut bindings = to_iter(bindings)?;

        let mut env = Env::inner(env);

        while let Some(key) = bindings.next() {
            let Some(value) = bindings.next() else {
                return Err(Error::UnevenArguments("let*"));
            };

            let key = to_sym(key)?;

            let value = env.eval(value)?;
            env.set(key, value);
        }

        env.eval(body)
    }

    pub fn def(env: &mut Env, args: List) -> Result<MalVal, Error> {
        let [key, value] = take_exact(args)?;

        let key = to_sym(key)?;

        let value = env.eval(value)?;
        env.set(key, value.clone());

        Ok(value)
    }

    pub fn r#do(env: &mut Env, args: List) -> Result<MalVal, Error> {
        let mut last = None;
        for value in args.into_iter() {
            last = Some(env.eval(value)?);
        }
        Ok(last.unwrap_or_else(|| MalVal::List(List::new())))
    }

    pub fn r#if(env: &mut Env, args: List) -> Result<MalVal, Error> {
        let [cond, truthy, falsey] = take_exact(args)?;

        let cond = match env.eval(cond)? {
            MalVal::List(list) => !list.is_empty(),
            MalVal::Bool(value) => value,
            _ => true,
        };

        if cond {
            env.eval(truthy)
        } else {
            env.eval(falsey)
        }
    }

    pub fn r#fn(env: &mut Env, args: List) -> Result<MalVal, Error> {
        let ([bindings, first], rest) = take_atleast(args)?;

        let bindings: Vec<String> = {
            let mut ret = vec![];
            for value in to_iter(bindings)? {
                ret.push(to_sym(value)?)
            }
            ret
        };

        Ok(MalVal::Fn {
            outer: env.clone(),
            bindings,
            body: (Box::new(first), rest),
        })
    }
}

pub fn ds(data: &mut HashMap<String, MalVal>) {
    data.insert("map".to_string(), MalVal::BuiltinFn(ds::map));
    data.insert("list".to_string(), MalVal::BuiltinFn(ds::list));
    data.insert("vec".to_string(), MalVal::BuiltinFn(ds::vec));
}

mod ds {
    use std::collections::HashMap;

    use crate::{
        env::Error,
        types::{List, MalKey, MalVal},
    };

    pub fn map(args: List) -> Result<MalVal, Error> {
        let mut args = args.into_iter();
        let mut map = HashMap::with_capacity(args.len() / 2);

        while let Some(key) = args.next() {
            let Some(value) = args.next() else {
                return Err(Error::UnevenArguments("map"));
            };

            let key = match MalKey::from_value(key) {
                Ok(key) => key,
                Err(key) => return Err(Error::InvalidMapKey(key.type_name())),
            };

            map.insert(key, value);
        }

        Ok(MalVal::Map(map))
    }

    pub fn list(args: List) -> Result<MalVal, Error> {
        Ok(MalVal::List(args))
    }

    pub fn vec(args: List) -> Result<MalVal, Error> {
        Ok(MalVal::Vector(args.into_vec()))
    }
}

pub fn math(data: &mut HashMap<String, MalVal>) {
    data.insert("+".to_string(), MalVal::BuiltinFn(math::add));
    data.insert("-".to_string(), MalVal::BuiltinFn(math::sub));
    data.insert("*".to_string(), MalVal::BuiltinFn(math::mul));
    data.insert("/".to_string(), MalVal::BuiltinFn(math::div));
}

mod math {
    use crate::{
        env::Error,
        types::{List, MalVal},
    };

    pub fn add(args: List) -> Result<MalVal, Error> {
        reduce(args, add2)
    }

    fn add2(fst: MalVal, snd: MalVal) -> Result<MalVal, Error> {
        match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float + (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a + b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a + b)),
            (MalVal::Str(a), MalVal::Str(b)) => Ok(MalVal::Str(format!("{a}{b}"))),
            (MalVal::List(mut a), MalVal::List(mut b)) => {
                a.append(&mut b);
                Ok(MalVal::List(a))
            }
            (MalVal::List(mut a), MalVal::Vector(b)) => {
                a.append(&mut List::from_vec(b));
                Ok(MalVal::List(a))
            }
            (MalVal::Vector(mut a), MalVal::Vector(mut b)) => {
                a.append(&mut b);
                Ok(MalVal::Vector(a))
            }
            (MalVal::Vector(mut a), MalVal::List(b)) => {
                a.append(&mut b.into_vec());
                Ok(MalVal::Vector(a))
            }
            vals => Err(error("+", vals)),
        }
    }

    pub fn sub(args: List) -> Result<MalVal, Error> {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float - (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a - b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a - b)),
            vals => Err(error("-", vals)),
        })
    }

    pub fn mul(args: List) -> Result<MalVal, Error> {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float * (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a * b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a * b)),
            vals => Err(error("*", vals)),
        })
    }

    pub fn div(args: List) -> Result<MalVal, Error> {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float / (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a / b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a / b)),
            vals => Err(error("/", vals)),
        })
    }

    fn reduce(
        args: List,
        join: impl Fn(MalVal, MalVal) -> Result<MalVal, Error>,
    ) -> Result<MalVal, Error> {
        let mut iter = args.into_iter();
        let Some(mut accum) = iter.next() else {
            return Ok(MalVal::List(List::new()));
        };

        for value in iter {
            accum = join(accum, value)?;
        }

        Ok(accum)
    }

    fn error(op: &'static str, (fst, snd): (MalVal, MalVal)) -> Error {
        Error::InvalidOperation {
            op,
            fst: fst.type_name(),
            snd: snd.type_name(),
        }
    }
}

fn to_sym(value: MalVal) -> Result<String, Error> {
    let MalVal::Sym(key) = value else {
        return Err(Error::UnexpectedType(MalVal::SYM, value.type_name()));
    };

    Ok(key)
}

fn to_iter(val: MalVal) -> Result<impl Iterator<Item = MalVal>, Error> {
    match val {
        MalVal::List(list) => Ok(ListLikeIter::List(list.into_iter())),
        MalVal::Vector(vec) => Ok(ListLikeIter::Vector(vec.into_iter())),
        _ => Err(Error::UnexpectedType("list-like", val.type_name())),
    }
}

pub enum ListLikeIter {
    List(<List as IntoIterator>::IntoIter),
    Vector(<Vec<MalVal> as IntoIterator>::IntoIter),
}

impl Iterator for ListLikeIter {
    type Item = MalVal;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ListLikeIter::List(list) => list.next(),
            ListLikeIter::Vector(vec) => vec.next(),
        }
    }
}

fn take_atleast<const N: usize>(mut list: List) -> Result<([MalVal; N], List), Error> {
    if list.len() < N {
        return Err(Error::AtleastArityMismatch(N, list.len()));
    }

    let rest = list.split_off(N);
    let array = list
        .into_vec()
        .try_into()
        .expect("split list should be the size of N");

    Ok((array, rest))
}

fn take_exact<const N: usize>(list: List) -> Result<[MalVal; N], Error> {
    match list.into_array::<N>() {
        Ok(arr) => Ok(arr),
        Err(list) => Err(Error::ArityMismatch(N, list.len())),
    }
}
