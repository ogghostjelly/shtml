use crate::{
    env::{Env, Error},
    types::{List, MalRet, MalVal},
};

pub fn std(data: &mut Env) {
    sform(data);
    math(data);
    ds(data);
    cmp(data);
    print(data);
}

pub fn sform(data: &mut Env) {
    data.set_special("let*", sform::r#let);
    data.set_special("def!", sform::def);
    data.set_special("do", sform::r#do);
    data.set_special("if", sform::r#if);
    data.set_special("fn*", sform::r#fn);

    data.set_special("quote", sform::quote);
    data.set_special("quasiquote", sform::quasiquote);
}

mod sform {
    use crate::{
        env::{Env, Error, TcoVal},
        ns::{take_atleast, to_iter},
        types::{List, MalVal, TcoRet},
    };

    use super::{take_exact, to_sym};

    pub fn quote(_: &mut Env, args: List) -> TcoRet {
        let [value] = take_exact(args)?;
        Ok(TcoVal::Val(value))
    }

    pub fn quasiquote(env: &mut Env, args: List) -> TcoRet {
        let [args] = take_exact(args)?;

        let MalVal::List(args) = args else {
            return Ok(TcoVal::Val(args));
        };

        quasiquote_inner(env, args)
    }

    pub fn quasiquote_inner(env: &mut Env, args: List) -> TcoRet {
        let args = match try_take1_sym(args, "unquote") {
            Ok(args) => {
                let [value] = take_exact(args)?;
                return Ok(TcoVal::Unevaluated(value));
            }
            Err(args) => args,
        };

        let mut new_list = List::new();

        for value in args.into_rev() {
            let MalVal::List(value) = value else {
                new_list.push_front(value);
                continue;
            };

            match try_take1_sym(value, "splice-unquote") {
                Ok(args) => {
                    let [value] = take_exact(args)?;
                    for el in to_iter(value)? {
                        new_list.push_front(el);
                    }
                }
                Err(value) => new_list.push_front({
                    let value = quasiquote_inner(env, value)?;
                    env.eval_tco(value)?
                }),
            }
        }

        Ok(TcoVal::Val(MalVal::List(new_list)))

        /*let mut new_list = List::new();

        for value in value.into_rev() {
            let MalVal::List(mut value) = value else {
                new_list.push_front(value);
                continue;
            };

            // Try take the first symbol
            // if that fails just push the value to new_list
            let Some(sym) = value.pop_front() else {
                new_list.push_front(MalVal::List(value));
                continue;
            };
            let MalVal::Sym(sym) = sym else {
                value.push_front(sym);
                let value = quasiquote(env, value)?;
                new_list.push_front(env.eval_tco(value)?);
                continue;
            };

            // Handle unquote/splice-unquote
            if sym == "unquote" {
                let [value] = take_exact(value)?;
                let value = env.eval(value)?;
                new_list.push_front(value);
            } else if sym == "splice-unquote" {
                let [value] = take_exact(value)?;
                for val in to_iter(env.eval(value)?)?.rev() {
                    new_list.push_front(val);
                }
            } else {
                let value = quasiquote(env, value)?;
                new_list.push_front(env.eval_tco(value)?);
            }
        }

        Ok(TcoVal::Val(MalVal::List(new_list)))*/
    }

    fn try_take1_sym(mut args: List, s: &str) -> Result<List, List> {
        let Some(first) = args.pop_front() else {
            return Err(args);
        };

        if let MalVal::Sym(sym) = first {
            if sym == s {
                return Ok(args);
            } else {
                args.push_front(MalVal::Sym(sym));
            }
        } else {
            args.push_front(first);
        }

        Err(args)
    }

    pub fn r#let(env: &mut Env, args: List) -> TcoRet {
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

        Ok(TcoVal::Unevaluated(body))
    }

    pub fn def(env: &mut Env, args: List) -> TcoRet {
        let [key, value] = take_exact(args)?;
        let key = to_sym(key)?;
        let mut value = env.eval(value)?;

        if let MalVal::Fn { name, .. } = &mut value {
            *name = Some(key.clone());
        }

        env.set(key, value.clone());

        Ok(TcoVal::Val(value))
    }

    pub fn r#do(env: &mut Env, args: List) -> TcoRet {
        let mut last = None;

        for value in args.into_iter() {
            if let Some(last) = last {
                env.eval(last)?;
            }

            last = Some(value);
        }

        match last {
            Some(last) => Ok(TcoVal::Unevaluated(last)),
            None => Ok(TcoVal::Val(MalVal::List(List::new()))),
        }
    }

    pub fn r#if(env: &mut Env, args: List) -> TcoRet {
        let [cond, truthy, falsey] = take_exact(args)?;

        if env.eval(cond)?.is_true() {
            Ok(TcoVal::Unevaluated(truthy))
        } else {
            Ok(TcoVal::Unevaluated(falsey))
        }
    }

    pub fn r#fn(env: &mut Env, args: List) -> TcoRet {
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

        Ok(TcoVal::Val(MalVal::Fn {
            name: None,
            outer: env.clone(),
            binds,
            bind_rest,
            body: (Box::new(first), rest),
        }))
    }
}

pub fn print(data: &mut Env) {
    data.set_fn("eprin", fmt::eprin);
    data.set_fn("eprint", fmt::eprint);
    data.set_fn("prin", fmt::prin);
    data.set_fn("print", fmt::print);
}

mod fmt {
    use std::fmt;

    use crate::types::{List, MalRet, MalVal};

    struct PrettyPrint(MalVal);

    impl fmt::Display for PrettyPrint {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self.0 {
                MalVal::Str(s) => s.fmt(f),
                _ => self.0.fmt(f),
            }
        }
    }

    pub fn prin(args: List) -> MalRet {
        for val in args {
            print!("{}", PrettyPrint(val));
        }
        Ok(MalVal::List(List::new()))
    }

    pub fn print(args: List) -> MalRet {
        for val in args {
            println!("{}", PrettyPrint(val));
        }
        Ok(MalVal::List(List::new()))
    }

    pub fn eprin(args: List) -> MalRet {
        for val in args {
            eprint!("{val}")
        }
        Ok(MalVal::List(List::new()))
    }

    pub fn eprint(args: List) -> MalRet {
        for val in args {
            eprintln!("{val}")
        }
        Ok(MalVal::List(List::new()))
    }
}

pub fn cmp(data: &mut Env) {
    data.set_fn("=", cmp::eq);
    data.set_fn("<", cmp::lt);
    data.set_fn("<=", cmp::lte);
    data.set_fn(">", cmp::gt);
    data.set_fn(">=", cmp::gte);

    data.set_fn("not", cmp::not);

    data.set_fn("list?", cmp::is_list);
    data.set_fn("empty?", cmp::is_empty);
}

mod cmp {
    use std::cmp::Ordering;

    use crate::{
        env::Error,
        types::{List, MalRet, MalVal},
    };

    use super::{all, all_reduce, take_exact};

    pub fn eq(args: List) -> MalRet {
        all_reduce(args, eq2)
    }

    pub fn not(args: List) -> MalRet {
        let [value] = take_exact(args)?;
        Ok(MalVal::Bool(!value.is_true()))
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

    pub fn lt(args: List) -> MalRet {
        cmp_num(args, |ord| matches!(ord, Ordering::Less))
    }

    pub fn lte(args: List) -> MalRet {
        cmp_num(args, |ord| matches!(ord, Ordering::Less | Ordering::Equal))
    }

    pub fn gt(args: List) -> MalRet {
        cmp_num(args, |ord| matches!(ord, Ordering::Greater))
    }

    pub fn gte(args: List) -> MalRet {
        cmp_num(args, |ord| {
            matches!(ord, Ordering::Greater | Ordering::Equal)
        })
    }

    fn cmp_num(args: List, cond: impl Fn(Ordering) -> bool) -> MalRet {
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

    pub fn is_list(args: List) -> MalRet {
        all(args, |value| Ok(matches!(value, MalVal::List(_))))
    }

    pub fn is_empty(args: List) -> MalRet {
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

pub fn ds(data: &mut Env) {
    data.set_fn("map", ds::map);
    data.set_fn("list", ds::list);
    data.set_fn("vec", ds::vec);

    data.set_fn("cons", ds::cons);
    data.set_fn("concat", ds::concat);

    data.set_fn("count", ds::count);
}

mod ds {
    use std::collections::HashMap;

    use crate::{
        env::Error,
        types::{List, MalKey, MalRet, MalVal},
    };

    use super::take_exact;

    pub fn cons(args: List) -> MalRet {
        let [el, value] = take_exact(args)?;

        let mut list = match value {
            MalVal::List(list) => list,
            MalVal::Vector(vec) => List::from_vec(vec),
            _ => return Err(Error::UnexpectedType(MalVal::LIST_LIKE, value.type_name())),
        };

        list.push_front(el);

        Ok(MalVal::List(list))
    }

    pub fn concat(args: List) -> MalRet {
        let mut list = List::new();

        for value in args.into_rev() {
            match value {
                MalVal::List(ls) => {
                    for el in ls.into_rev() {
                        list.push_front(el);
                    }
                }
                MalVal::Vector(vec) => {
                    for el in vec.into_iter().rev() {
                        list.push_front(el);
                    }
                }
                _ => return Err(Error::UnexpectedType(MalVal::LIST_LIKE, value.type_name())),
            }
        }

        Ok(MalVal::List(list))
    }

    pub fn map(args: List) -> MalRet {
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

    pub fn list(args: List) -> MalRet {
        Ok(MalVal::List(args))
    }

    pub fn vec(args: List) -> MalRet {
        Ok(MalVal::Vector(args.into_vec()))
    }

    pub fn count(args: List) -> MalRet {
        let [value] = take_exact(args)?;

        Ok(MalVal::Int(match value {
            MalVal::List(list) => list.len(),
            MalVal::Vector(vec) => vec.len(),
            MalVal::Map(map) => map.len(),
            _ => return Err(Error::InvalidOperation1("count", value.type_name())),
        } as i64))
    }
}

pub fn math(data: &mut Env) {
    data.set_fn("+", math::add);
    data.set_fn("-", math::sub);
    data.set_fn("*", math::mul);
    data.set_fn("/", math::div);
}

mod math {
    use crate::types::{List, MalRet, MalVal};

    use super::{op_error, reduce};

    pub fn add(args: List) -> MalRet {
        reduce(args, add2)
    }

    fn add2(fst: MalVal, snd: MalVal) -> MalRet {
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

    pub fn sub(args: List) -> MalRet {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float - (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a - b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a - b)),
            vals => Err(op_error("-", vals)),
        })
    }

    pub fn mul(args: List) -> MalRet {
        reduce(args, |fst, snd| match (fst, snd) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float * (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a * b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a * b)),
            vals => Err(op_error("*", vals)),
        })
    }

    pub fn div(args: List) -> MalRet {
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

fn to_iter(val: MalVal) -> Result<ListLikeIter, Error> {
    match val {
        MalVal::List(list) => Ok(ListLikeIter::List(list.into_iter())),
        MalVal::Vector(vec) => Ok(ListLikeIter::Vector(vec.into_iter())),
        _ => Err(Error::UnexpectedType(MalVal::LIST_LIKE, val.type_name())),
    }
}

fn all(args: List, cond: impl Fn(MalVal) -> Result<bool, Error>) -> MalRet {
    for value in args {
        if !cond(value)? {
            return Ok(MalVal::Bool(false));
        }
    }
    Ok(MalVal::Bool(true))
}

fn all_reduce(args: List, cond: impl Fn(&MalVal, &MalVal) -> Result<bool, Error>) -> MalRet {
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

fn reduce(args: List, join: impl Fn(MalVal, MalVal) -> MalRet) -> MalRet {
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

impl DoubleEndedIterator for ListLikeIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self {
            ListLikeIter::List(list) => list.next_back(),
            ListLikeIter::Vector(vec) => vec.next_back(),
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

fn take_exact<const N: usize>(args: List) -> Result<[MalVal; N], Error> {
    match args.into_array::<N>() {
        Ok(arr) => Ok(arr),
        Err(list) => Err(Error::ArityMismatch(N, list.len())),
    }
}
