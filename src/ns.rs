use crate::{
    env::Env,
    list, loc,
    reader::{self, Location},
    types::{List, MalData, MalVal},
    Error, ErrorKind, MalRet,
};

pub fn std(data: &mut Env) {
    sform(data);
    math(data);
    ds(data);
    cmp(data);
    print(data);
    std_mal(data);
}

fn std_mal(data: &mut Env) {
    let ast = reader::parse(Location::file("std.mal"), include_str!("std.mal"))
        .expect("parsing 'lib.mal' should never fail");

    if let Some(ast) = ast {
        _ = data
            .eval(ast)
            .expect("evaluating 'lib.mal' should never fail")
    }
}

pub fn sform(data: &mut Env) {
    data.set_special(loc!(), "let*", sform::r#let);
    data.set_special(loc!(), "def!", sform::def);
    data.set_special(loc!(), "do", sform::r#do);
    data.set_special(loc!(), "if", sform::r#if);
    data.set_special(loc!(), "fn*", sform::r#fn);
    data.set_fn(loc!(), "macro", sform::r#macro);

    data.set_special(loc!(), "quote", sform::quote);
    data.set_special(loc!(), "quasiquote", sform::quasiquote);
}

mod sform {
    use crate::{
        env::{Env, TcoRet, TcoVal},
        list,
        ns::take_atleast,
        reader::Location,
        types::{List, MalFn, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{take_exact, to_list_like, to_sym};

    pub fn quote(_: &mut Env, loc: Location, args: List) -> TcoRet {
        let [value] = take_exact("quote", loc, args)?;
        Ok(TcoVal::Val(value))
    }

    pub fn quasiquote(env: &mut Env, loc: Location, args: List) -> TcoRet {
        let [args] = take_exact("quasiquote", loc.clone(), args)?;

        let MalVal::List(args) = args.value else {
            return Ok(TcoVal::Val(args));
        };

        quasiquote_inner(env, loc, args)
    }

    pub fn quasiquote_inner(env: &mut Env, loc: Location, args: List) -> TcoRet {
        let args = match try_take1_sym(args, "unquote") {
            Ok(args) => {
                let [value] = take_exact("unquote", loc, args)?;
                return Ok(TcoVal::Unevaluated(value));
            }
            Err(args) => args,
        };

        let mut new_list = List::new();

        for data in args.into_rev() {
            let MalVal::List(value) = data.value else {
                new_list.push_front(data);
                continue;
            };

            match try_take1_sym(value, "splice-unquote") {
                Ok(args) => {
                    let [value] = take_exact("splice-unquote", data.loc, args)?;
                    let value = env.eval(value)?;
                    for el in to_list_like("splice-unquote", value)? {
                        new_list.push_front(el);
                    }
                }
                Err(value) => new_list.push_front({
                    let value = quasiquote_inner(env, data.loc, value)?;
                    env.eval_tco(value)?
                }),
            }
        }

        Ok(TcoVal::Val(MalVal::List(new_list).with_loc(loc)))
    }

    fn try_take1_sym(mut args: List, s: &str) -> Result<List, List> {
        let Some(first) = args.pop_front() else {
            return Err(args);
        };

        if let MalVal::Sym(sym) = first.value {
            if sym == s {
                return Ok(args);
            } else {
                args.push_front(MalVal::Sym(sym).with_loc(first.loc));
            }
        } else {
            args.push_front(first);
        }

        Err(args)
    }

    pub fn r#let(env: &mut Env, loc: Location, args: List) -> TcoRet {
        let [bindings, body] = take_exact("let*", loc.clone(), args)?;
        let mut bindings = to_list_like("let*", bindings)?.into_iter();

        let mut env = Env::inner(env);

        while let Some(key) = bindings.next() {
            let Some(value) = bindings.next() else {
                return Err(Error::new(loc, ErrorKind::UnevenArguments("let*")));
            };

            let key = to_sym("let*", key)?;

            let value = env.eval(value)?;
            env.set(key, value);
        }

        Ok(TcoVal::Unevaluated(body))
    }

    pub fn def(env: &mut Env, loc: Location, args: List) -> TcoRet {
        let [key, value] = take_exact("def!", loc.clone(), args)?;
        let key = to_sym("def!", key)?;
        let mut value = env.eval(value)?;

        if let MalVal::Fn(MalFn { name, .. }) = &mut value.value {
            *name = Some(key.clone());
        }

        env.set(key, value);

        Ok(TcoVal::Val(list!().with_loc(loc)))
    }

    pub fn r#do(env: &mut Env, loc: Location, args: List) -> TcoRet {
        let mut last = None;

        for value in args.into_iter() {
            if let Some(last) = last {
                env.eval(last)?;
            }

            last = Some(value);
        }

        match last {
            Some(last) => Ok(TcoVal::Unevaluated(last)),
            None => Ok(TcoVal::Val(list!().with_loc(loc))),
        }
    }

    pub fn r#if(env: &mut Env, loc: Location, args: List) -> TcoRet {
        let [cond, truthy, falsey] = take_exact("if", loc, args)?;

        if env.eval(cond)?.is_true() {
            Ok(TcoVal::Unevaluated(truthy))
        } else {
            Ok(TcoVal::Unevaluated(falsey))
        }
    }

    pub fn r#fn(env: &mut Env, loc: Location, args: List) -> TcoRet {
        let ([bindings, first], rest) = take_atleast("fn*", loc.clone(), args)?;

        let mut binds: Vec<String> = vec![];
        let mut bind_rest: Option<Option<(Location, String)>> = None;

        for value in to_list_like("fn*", bindings)?.into_iter() {
            let loc = value.loc.clone();
            let sym = to_sym("fn*", value)?;

            match &bind_rest {
                Some(None) => bind_rest = Some(Some((loc, sym))),
                Some(Some(_)) => return Err(Error::new(loc, ErrorKind::BindsAfterRest)),
                None => {
                    if sym == "&" {
                        bind_rest = Some(None);
                    } else {
                        binds.push(sym);
                    }
                }
            }
        }

        Ok(TcoVal::Val(
            MalVal::Fn(MalFn {
                name: None,
                is_macro: false,
                outer: env.clone(),
                binds,
                bind_rest,
                body: (Box::new(first), rest),
            })
            .with_loc(loc),
        ))
    }

    pub fn r#macro(loc: Location, args: List) -> MalRet {
        let [value] = take_exact("macro", loc.clone(), args)?;

        let MalVal::Fn(mut f) = value.value else {
            return Err(Error::new(
                loc,
                ErrorKind::UnexpectedType(MalVal::FN, value.type_name(), "macro".into()),
            ));
        };

        f.is_macro = true;

        Ok(MalVal::Fn(f).with_loc(loc))
    }
}

pub fn print(data: &mut Env) {
    data.set_fn(loc!(), "prin", fmt::prin);
    data.set_fn(loc!(), "print", fmt::print);
}

mod fmt {
    use std::fmt;

    use crate::{
        reader::Location,
        types::{List, MalVal},
        MalRet,
    };

    use super::take_exact;

    struct PrettyPrint<'a>(&'a MalVal);

    impl fmt::Display for PrettyPrint<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self.0 {
                MalVal::Str(s) => s.fmt(f),
                _ => self.0.fmt(f),
            }
        }
    }

    pub fn prin(loc: Location, args: List) -> MalRet {
        let [value] = take_exact("prin", loc, args)?;
        print!("{}", PrettyPrint(&value.value));
        Ok(value)
    }

    pub fn print(loc: Location, args: List) -> MalRet {
        let [value] = take_exact("print", loc, args)?;
        println!("{}", PrettyPrint(&value.value));
        Ok(value)
    }
}

pub fn cmp(data: &mut Env) {
    data.set_fn(loc!(), "=", cmp::eq);
    data.set_fn(loc!(), "<", cmp::lt);
    data.set_fn(loc!(), "<=", cmp::lte);
    data.set_fn(loc!(), ">", cmp::gt);
    data.set_fn(loc!(), ">=", cmp::gte);

    data.set_fn(loc!(), "not", cmp::not);

    data.set_fn(loc!(), "list?", cmp::is_list);
    data.set_fn(loc!(), "empty?", cmp::is_empty);
}

mod cmp {
    use std::cmp::Ordering;

    use crate::{
        reader::Location,
        types::{List, MalData, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{all, all_reduce, take_exact};

    pub fn eq(loc: Location, args: List) -> MalRet {
        all_reduce(loc, args, eq2)
    }

    pub fn not(loc: Location, args: List) -> MalRet {
        let [value] = take_exact("not", loc.clone(), args)?;
        Ok(MalVal::Bool(!value.is_true()).with_loc(loc))
    }

    pub fn eq2(fst: &MalData, snd: &MalData) -> Result<bool, Error> {
        Ok(match (&fst.value, &snd.value) {
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
        T: ExactSizeIterator + Iterator<Item = &'a MalData>,
        U: ExactSizeIterator + Iterator<Item = &'a MalData>,
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

    pub fn lt(loc: Location, args: List) -> MalRet {
        cmp_num(loc, args, |ord| matches!(ord, Ordering::Less))
    }

    pub fn lte(loc: Location, args: List) -> MalRet {
        cmp_num(loc, args, |ord| {
            matches!(ord, Ordering::Less | Ordering::Equal)
        })
    }

    pub fn gt(loc: Location, args: List) -> MalRet {
        cmp_num(loc, args, |ord| matches!(ord, Ordering::Greater))
    }

    pub fn gte(loc: Location, args: List) -> MalRet {
        cmp_num(loc, args, |ord| {
            matches!(ord, Ordering::Greater | Ordering::Equal)
        })
    }

    fn cmp_num(loc: Location, args: List, cond: impl Fn(Ordering) -> bool) -> MalRet {
        all_reduce(loc, args, |fst, snd| {
            let ord = match (&fst.value, &snd.value) {
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

    pub fn is_list(loc: Location, args: List) -> MalRet {
        all(loc, args, |value| {
            Ok(matches!(value.value, MalVal::List(_)))
        })
    }

    pub fn is_empty(loc: Location, args: List) -> MalRet {
        all(loc, args, |value| {
            Ok(match value.value {
                MalVal::List(list) => list.is_empty(),
                MalVal::Vector(vec) => vec.is_empty(),
                MalVal::Map(map) => map.is_empty(),
                _ => {
                    return {
                        let kind = ErrorKind::InvalidOperation1("empty?", value.type_name());
                        Err(Error::new(value.loc, kind))
                    }
                }
            })
        })
    }
}

pub fn ds(data: &mut Env) {
    data.set_fn(loc!(), "hash-map", ds::hash_map);
    data.set_fn(loc!(), "list", ds::list);
    data.set_fn(loc!(), "vec", ds::vec);

    data.set_fn(loc!(), "cons", ds::cons);
    data.set_fn(loc!(), "concat", ds::concat);

    data.set_fn(loc!(), "first", ds::first);
    data.set_fn(loc!(), "rest", ds::rest);
    data.set_fn(loc!(), "nth", ds::nth);

    data.set_fn(loc!(), "count", ds::count);
}

mod ds {
    use indexmap::IndexMap;

    use crate::{
        reader::Location,
        types::{List, MalKey, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{take_exact, to_list_like};

    pub fn nth(loc: Location, args: List) -> MalRet {
        let [value, index] = take_exact("nth", loc.clone(), args)?;

        let mut value = to_list_like("nth", value)?;

        let MalVal::Int(index) = index.value else {
            let kind = ErrorKind::UnexpectedType(MalVal::INT, index.type_name(), "nth".into());
            return Err(Error::new(index.loc, kind));
        };

        if index < 0 || index >= value.len() as i64 {
            return Err(Error::new(
                loc,
                ErrorKind::IndexOutOfRange(index, value.len()),
            ));
        }

        Ok(value.swap_remove(index as usize))
    }

    pub fn first(loc: Location, args: List) -> MalRet {
        let [value] = take_exact("first", loc.clone(), args)?;
        let mut value = to_list_like("first", value)?.to_list();
        let Some(value) = value.pop_front() else {
            return Err(Error::new(loc, ErrorKind::FirstOfEmptyList));
        };
        Ok(value)
    }

    pub fn rest(loc: Location, args: List) -> MalRet {
        let [value] = take_exact("rest", loc.clone(), args)?;
        let mut ls = to_list_like("rest", value)?.to_list();
        ls.pop_front();
        Ok(MalVal::List(ls).with_loc(loc))
    }

    pub fn cons(loc: Location, args: List) -> MalRet {
        let [el, value] = take_exact("cons", loc.clone(), args)?;

        let mut list = to_list_like("cons", value)?.to_list();

        list.push_front(el);

        Ok(MalVal::List(list).with_loc(loc))
    }

    pub fn concat(loc: Location, args: List) -> MalRet {
        let mut list = List::new();

        for value in args.into_rev() {
            for el in to_list_like("concat", value)?.into_iter().rev() {
                list.push_front(el);
            }
        }

        Ok(MalVal::List(list).with_loc(loc))
    }

    pub fn hash_map(loc: Location, args: List) -> MalRet {
        let mut args = args.into_iter();
        let mut map = IndexMap::with_capacity(args.len() / 2);

        while let Some(key) = args.next() {
            let Some(value) = args.next() else {
                return Err(Error::new(loc, ErrorKind::UnevenArguments("hash-map")));
            };

            let key = match MalKey::from_value(key.value) {
                Ok(key) => key,
                Err(key) => return Err(Error::new(loc, ErrorKind::InvalidMapKey(key.type_name()))),
            };

            map.insert(key, value);
        }

        Ok(MalVal::Map(map).with_loc(loc))
    }

    pub fn list(loc: Location, args: List) -> MalRet {
        Ok(MalVal::List(args).with_loc(loc))
    }

    pub fn vec(loc: Location, args: List) -> MalRet {
        Ok(MalVal::Vector(args.into_vec()).with_loc(loc))
    }

    pub fn count(loc: Location, args: List) -> MalRet {
        let [value] = take_exact("count", loc.clone(), args)?;

        Ok(MalVal::Int(match value.value {
            MalVal::List(list) => list.len(),
            MalVal::Vector(vec) => vec.len(),
            MalVal::Map(map) => map.len(),
            _ => {
                return Err(Error::new(
                    loc,
                    ErrorKind::InvalidOperation1("count", value.type_name()),
                ))
            }
        } as i64)
        .with_loc(loc))
    }
}

pub fn math(data: &mut Env) {
    data.set_fn(loc!(), "+", math::add);
    data.set_fn(loc!(), "-", math::sub);
    data.set_fn(loc!(), "*", math::mul);
    data.set_fn(loc!(), "/", math::div);
}

mod math {
    use crate::{
        reader::Location,
        types::{List, MalData, MalVal},
        Error, MalRet,
    };

    use super::{op_error, reduce};

    pub fn add(loc: Location, args: List) -> MalRet {
        reduce(loc, args, add2)
    }

    fn add2(fst: MalData, snd: MalData) -> Result<MalVal, Error> {
        match (fst.value, snd.value) {
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
            vals => Err(op_error(fst.loc, "+", vals)),
        }
    }

    pub fn sub(loc: Location, args: List) -> MalRet {
        reduce(loc, args, |fst, snd| match (fst.value, snd.value) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float - (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a - b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a - b)),
            vals => Err(op_error(fst.loc, "-", vals)),
        })
    }

    pub fn mul(loc: Location, args: List) -> MalRet {
        reduce(loc, args, |fst, snd| match (fst.value, snd.value) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float * (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a * b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a * b)),
            vals => Err(op_error(fst.loc, "*", vals)),
        })
    }

    pub fn div(loc: Location, args: List) -> MalRet {
        reduce(loc, args, |fst, snd| match (fst.value, snd.value) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float / (int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a / b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a / b)),
            vals => Err(op_error(fst.loc, "/", vals)),
        })
    }
}

fn op_error(loc: Location, op: &'static str, (fst, snd): (MalVal, MalVal)) -> Error {
    Error::new(
        loc,
        ErrorKind::InvalidOperation {
            op,
            fst: fst.type_name(),
            snd: snd.type_name(),
        },
    )
}

fn to_sym(name: impl Into<String>, value: MalData) -> Result<String, Error> {
    let MalVal::Sym(key) = value.value else {
        return {
            let kind = ErrorKind::UnexpectedType(MalVal::SYM, value.type_name(), name.into());
            Err(Error::new(value.loc, kind))
        };
    };

    Ok(key)
}

fn to_list_like(name: impl Into<String>, val: MalData) -> Result<ListLike, Error> {
    match val.value {
        MalVal::List(list) => Ok(ListLike::List(list)),
        MalVal::Vector(vec) => Ok(ListLike::Vector(vec)),
        _ => {
            let kind = ErrorKind::UnexpectedType(MalVal::LIST_LIKE, val.type_name(), name.into());
            Err(Error::new(val.loc, kind))
        }
    }
}

fn all(loc: Location, args: List, cond: impl Fn(MalData) -> Result<bool, Error>) -> MalRet {
    for value in args {
        if !cond(value)? {
            return Ok(MalVal::Bool(false).with_loc(loc));
        }
    }
    Ok(MalVal::Bool(true).with_loc(loc))
}

fn all_reduce(
    loc: Location,
    args: List,
    cond: impl Fn(&MalData, &MalData) -> Result<bool, Error>,
) -> MalRet {
    let mut iter = args.into_iter();
    let Some(mut last) = iter.next() else {
        return Ok(list!().with_loc(loc));
    };

    for value in iter {
        if !cond(&last, &value)? {
            return Ok(MalVal::Bool(false).with_loc(loc));
        }

        last = value;
    }

    Ok(MalVal::Bool(true).with_loc(loc))
}

fn reduce(
    loc: Location,
    args: List,
    join: impl Fn(MalData, MalData) -> Result<MalVal, Error>,
) -> MalRet {
    let mut iter = args.into_iter();
    let Some(mut accum) = iter.next() else {
        return Ok(list!().with_loc(loc));
    };

    for value in iter {
        accum = join(accum, value)?.with_loc(loc.clone());
    }

    Ok(accum)
}

pub enum ListLike {
    List(List),
    Vector(Vec<MalData>),
}

impl ListLike {
    pub fn to_list(self) -> List {
        match self {
            ListLike::List(list) => list,
            ListLike::Vector(vec) => List::from_vec(vec),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            ListLike::List(list) => list.len(),
            ListLike::Vector(vec) => vec.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            ListLike::List(list) => list.is_empty(),
            ListLike::Vector(vec) => vec.is_empty(),
        }
    }

    pub fn swap_remove(&mut self, index: usize) -> MalData {
        match self {
            ListLike::List(list) => list.swap_remove(index),
            ListLike::Vector(vec) => vec.swap_remove(index),
        }
    }
}

impl IntoIterator for ListLike {
    type Item = MalData;
    type IntoIter = ListLikeIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            ListLike::List(list) => ListLikeIter::List(list.into_iter()),
            ListLike::Vector(vec) => ListLikeIter::Vector(vec.into_iter()),
        }
    }
}

pub enum ListLikeIter {
    List(<List as IntoIterator>::IntoIter),
    Vector(<Vec<MalData> as IntoIterator>::IntoIter),
}

impl Iterator for ListLikeIter {
    type Item = MalData;

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

fn take_atleast<const N: usize>(
    name: impl Into<String>,
    loc: Location,
    mut list: List,
) -> Result<([MalData; N], List), Error> {
    if list.len() < N {
        return Err(Error::new(
            loc,
            ErrorKind::AtleastArityMismatch(N, list.len(), name.into()),
        ));
    }

    let rest = list.split_off(N);
    let array = list
        .into_vec()
        .try_into()
        .expect("split list should be the size of N");

    Ok((array, rest))
}

fn take_exact<const N: usize>(
    name: impl Into<String>,
    loc: Location,
    args: List,
) -> Result<[MalData; N], Error> {
    match args.into_array::<N>() {
        Ok(arr) => Ok(arr),
        Err(list) => Err(Error::new(
            loc,
            ErrorKind::ArityMismatch(N, list.len(), name.into()),
        )),
    }
}
