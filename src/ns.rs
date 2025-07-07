use std::collections::HashMap;

use crate::{
    env::Error,
    types::{List, MalVal},
};

pub fn std(data: &mut HashMap<String, MalVal>) {
    sform(data);
    math(data);
    ds(data);
    cmp(data);
    print(data);
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

        let mut binds: Vec<String> = vec![];
        let mut bind_rest: Option<Option<String>> = None;

        for value in to_iter(bindings)? {
            let sym = to_sym(value)?;

            match &bind_rest {
                Some(None) => bind_rest = Some(Some(sym)),
                Some(Some(_)) => return Err(Error::BindsAfterRest),
                None => {
                    if sym == "&" {
                        bind_rest = Some(None);
                    } else {
                        binds.push(sym);
                    }
                }
            }
        }

        Ok(MalVal::Fn {
            outer: env.clone(),
            binds,
            bind_rest,
            body: (Box::new(first), rest),
        })
    }
}

pub fn print(data: &mut HashMap<String, MalVal>) {
    data.insert("prn".to_string(), MalVal::BuiltinFn(print::prn));
    data.insert("print".to_string(), MalVal::BuiltinFn(print::print));
}

mod print {
    use crate::{
        env::Error,
        types::{List, MalVal},
    };

    pub fn prn(args: List) -> Result<MalVal, Error> {
        for val in args {
            print!("{val}")
        }
        Ok(MalVal::List(List::new()))
    }

    pub fn print(args: List) -> Result<MalVal, Error> {
        for val in args {
            println!("{val}")
        }
        Ok(MalVal::List(List::new()))
    }
}

pub fn cmp(data: &mut HashMap<String, MalVal>) {
    data.insert("=".to_string(), MalVal::BuiltinFn(cmp::eq));
    data.insert("<".to_string(), MalVal::BuiltinFn(cmp::lt));
    data.insert("<=".to_string(), MalVal::BuiltinFn(cmp::lte));
    data.insert(">".to_string(), MalVal::BuiltinFn(cmp::gt));
    data.insert(">=".to_string(), MalVal::BuiltinFn(cmp::gte));

    data.insert("list?".to_string(), MalVal::BuiltinFn(cmp::is_list));
    data.insert("empty?".to_string(), MalVal::BuiltinFn(cmp::is_empty));
}

mod cmp {
    use std::cmp::Ordering;

    use crate::{
        env::Error,
        types::{List, MalVal},
    };

    use super::{all, all_reduce};

    pub fn eq(args: List) -> Result<MalVal, Error> {
        all_reduce(args, eq2)
    }

    pub fn eq2(fst: &MalVal, snd: &MalVal) -> Result<bool, Error> {
        Ok(match (fst, snd) {
            (MalVal::List(fst), MalVal::List(snd)) => eq_iter(fst.iter(), snd.iter())?,
            (MalVal::Vector(fst), MalVal::Vector(snd)) => eq_iter(fst.iter(), snd.iter())?,
            (MalVal::List(fst), MalVal::Vector(snd)) => eq_iter(fst.iter(), snd.iter())?,
            (MalVal::Vector(fst), MalVal::List(snd)) => eq_iter(fst.iter(), snd.iter())?,

            (MalVal::Map(fst), MalVal::Map(snd)) => {
                if fst.len() != snd.len() {
                    return Ok(false);
                }

                for (key, fst) in fst {
                    let Some(snd) = snd.get(key) else {
                        return Ok(false);
                    };

                    if !eq2(fst, snd)? {
                        return Ok(false);
                    }
                }

                true
            }

            (MalVal::Sym(fst), MalVal::Sym(snd)) => fst == snd,
            (MalVal::Str(fst), MalVal::Str(snd)) => fst == snd,
            (MalVal::Kwd(fst), MalVal::Kwd(snd)) => fst == snd,

            (MalVal::Int(fst), MalVal::Int(snd)) => fst == snd,
            (MalVal::Float(fst), MalVal::Float(snd)) => fst == snd,
            (MalVal::Float(float), MalVal::Int(int)) | (MalVal::Int(int), MalVal::Float(float)) => {
                *float == (*int as f64)
            }

            (MalVal::Bool(fst), MalVal::Bool(snd)) => fst == snd,
            _ => false,
        })
    }

    fn eq_iter<'a, T, U>(fst: T, snd: U) -> Result<bool, Error>
    where
        T: ExactSizeIterator + Iterator<Item = &'a MalVal>,
        U: ExactSizeIterator + Iterator<Item = &'a MalVal>,
    {
        if fst.len() != snd.len() {
            return Ok(false);
        }

        for (fst, snd) in fst.zip(snd) {
            if !eq2(fst, snd)? {
                return Ok(false);
            }
        }

        Ok(true)
    }

    pub fn lt(args: List) -> Result<MalVal, Error> {
        cmp_num(args, |ord| matches!(ord, Ordering::Less))
    }

    pub fn lte(args: List) -> Result<MalVal, Error> {
        cmp_num(args, |ord| matches!(ord, Ordering::Less | Ordering::Equal))
    }

    pub fn gt(args: List) -> Result<MalVal, Error> {
        cmp_num(args, |ord| matches!(ord, Ordering::Greater))
    }

    pub fn gte(args: List) -> Result<MalVal, Error> {
        cmp_num(args, |ord| {
            matches!(ord, Ordering::Greater | Ordering::Equal)
        })
    }

    fn cmp_num(args: List, cond: impl Fn(Ordering) -> bool) -> Result<MalVal, Error> {
        all_reduce(args, |fst, snd| {
            let ord = match (fst, snd) {
                (MalVal::Int(fst), MalVal::Int(snd)) => fst.partial_cmp(snd),
                (MalVal::Float(fst), MalVal::Float(snd)) => fst.partial_cmp(snd),
                (MalVal::Float(float), MalVal::Int(int))
                | (MalVal::Int(int), MalVal::Float(float)) => float.partial_cmp(&(*int as f64)),
                _ => None,
            };

            match ord {
                Some(ord) => Ok(cond(ord)),
                None => Ok(false),
            }
        })
    }

    pub fn is_list(args: List) -> Result<MalVal, Error> {
        all(args, |value| Ok(matches!(value, MalVal::List(_))))
    }

    pub fn is_empty(args: List) -> Result<MalVal, Error> {
        all(args, |value| {
            Ok(match value {
                MalVal::List(list) => list.is_empty(),
                MalVal::Vector(vec) => vec.is_empty(),
                MalVal::Map(map) => map.is_empty(),
                _ => return Err(Error::InvalidOperation1("empty?", value.type_name())),
            })
        })
    }
}

pub fn ds(data: &mut HashMap<String, MalVal>) {
    data.insert("map".to_string(), MalVal::BuiltinFn(ds::map));
    data.insert("list".to_string(), MalVal::BuiltinFn(ds::list));
    data.insert("vec".to_string(), MalVal::BuiltinFn(ds::vec));

    data.insert("count".to_string(), MalVal::BuiltinFn(ds::count));
}

mod ds {
    use std::collections::HashMap;

    use crate::{
        env::Error,
        types::{List, MalKey, MalVal},
    };

    use super::take_exact;

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

    pub fn count(args: List) -> Result<MalVal, Error> {
        let [value] = take_exact(args)?;

        Ok(MalVal::Int(match value {
            MalVal::List(list) => list.len(),
            MalVal::Vector(vec) => vec.len(),
            MalVal::Map(map) => map.len(),
            _ => return Err(Error::InvalidOperation1("count", value.type_name())),
        } as i64))
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

    use super::{op_error, reduce};

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
            vals => Err(op_error("+", vals)),
        }
    }

    pub fn sub(args: List) -> Result<MalVal, Error> {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float - (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a - b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a - b)),
            vals => Err(op_error("-", vals)),
        })
    }

    pub fn mul(args: List) -> Result<MalVal, Error> {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float * (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a * b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a * b)),
            vals => Err(op_error("*", vals)),
        })
    }

    pub fn div(args: List) -> Result<MalVal, Error> {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float / (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a / b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a / b)),
            vals => Err(op_error("/", vals)),
        })
    }
}

fn op_error(op: &'static str, (fst, snd): (MalVal, MalVal)) -> Error {
    Error::InvalidOperation {
        op,
        fst: fst.type_name(),
        snd: snd.type_name(),
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

fn all(args: List, cond: impl Fn(MalVal) -> Result<bool, Error>) -> Result<MalVal, Error> {
    for value in args {
        if !cond(value)? {
            return Ok(MalVal::Bool(false));
        }
    }
    Ok(MalVal::Bool(true))
}

fn all_reduce(
    args: List,
    cond: impl Fn(&MalVal, &MalVal) -> Result<bool, Error>,
) -> Result<MalVal, Error> {
    let mut iter = args.into_iter();
    let Some(mut last) = iter.next() else {
        return Ok(MalVal::List(List::new()));
    };

    for value in iter {
        if !cond(&last, &value)? {
            return Ok(MalVal::Bool(false));
        }

        last = value;
    }

    Ok(MalVal::Bool(true))
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
