use crate::{
    env::Env,
    list, loc,
    reader::{self, Location},
    types::{CallContext, List, MalData, MalVal},
    Error, ErrorKind, MalRet,
};

pub fn std(data: &mut Env) {
    sform(data);
    math(data);
    ds(data);
    cmp(data);
    fmt(data);
    std_mal(data);
    fs(data);
}

fn std_mal(data: &mut Env) {
    let ast = reader::parse(Location::file("std.mal"), include_str!("std.mal"))
        .expect("parsing 'lib.mal' should never fail");

    for value in ast {
        _ = data
            .eval(&CallContext::std(), value)
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
        types::{CallContext, List, MalFn, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{take_exact, to_list_like, to_sym};

    pub fn quote(ctx: &CallContext, _: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [value] = take_exact(ctx, &loc, args)?;
        Ok(TcoVal::Val(value))
    }

    pub fn quasiquote(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [args] = take_exact(ctx, &loc, args)?;

        let MalVal::List(args) = args.value else {
            return Ok(TcoVal::Val(args));
        };

        quasiquote_inner(ctx, env, (args, loc))
    }

    pub fn quasiquote_inner(
        ctx: &CallContext,
        env: &mut Env,
        (list, loc): (List, Location),
    ) -> TcoRet {
        let args = match try_take1_sym(list, "unquote") {
            Ok(args) => {
                let ctx = ctx.new_frame(("unquote".into(), loc.clone()));
                let [value] = take_exact(&ctx, &loc, args)?;
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
                    let ctx = ctx.new_frame(("unquote".into(), data.loc.clone()));
                    let [value] = take_exact(&ctx, &data.loc, args)?;
                    let value = env.eval(&ctx, value)?;
                    for el in to_list_like(&ctx, value)? {
                        new_list.push_front(el);
                    }
                }
                Err(value) => new_list.push_front({
                    let value = quasiquote_inner(ctx, env, (value, data.loc))?;
                    env.eval_tco(ctx, value)?
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

    pub fn r#let(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [bindings, body] = take_exact(ctx, &loc, args)?;
        let mut bindings = to_list_like(ctx, bindings)?.into_iter();

        let mut env = Env::inner(env);

        while let Some(key) = bindings.next() {
            let Some(value) = bindings.next() else {
                return Err(Error::new(ErrorKind::UnevenArguments("let*"), ctx, key.loc));
            };

            let key = to_sym(ctx, key)?;

            let value = env.eval(ctx, value)?;
            env.set(key, value);
        }

        Ok(TcoVal::Unevaluated(body))
    }

    pub fn def(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [key, value] = take_exact(ctx, &loc, args)?;
        let key = to_sym(ctx, key)?;
        let mut value = env.eval(ctx, value)?;

        if let MalVal::Fn(MalFn { name, .. }) = &mut value.value {
            *name = Some(key.clone());
        }

        env.set(key, value);

        Ok(TcoVal::Val(list!().with_loc(loc)))
    }

    pub fn r#do(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let mut last = None;

        for value in args.into_iter() {
            if let Some(last) = last {
                env.eval(ctx, last)?;
            }

            last = Some(value);
        }

        match last {
            Some(last) => Ok(TcoVal::Unevaluated(last)),
            None => Ok(TcoVal::Val(list!().with_loc(loc))),
        }
    }

    pub fn r#if(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [cond, truthy, falsey] = take_exact(ctx, &loc, args)?;

        if env.eval(ctx, cond)?.is_true() {
            Ok(TcoVal::Unevaluated(truthy))
        } else {
            Ok(TcoVal::Unevaluated(falsey))
        }
    }

    pub fn r#fn(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let ([bindings, first], rest) = take_atleast(ctx, &loc, args)?;

        let mut binds: Vec<String> = vec![];
        let mut bind_rest: Option<Option<(Location, String)>> = None;

        for value in to_list_like(ctx, bindings)?.into_iter() {
            let loc = value.loc.clone();
            let sym = to_sym(ctx, value)?;

            match &bind_rest {
                Some(None) => bind_rest = Some(Some((loc, sym))),
                Some(Some(_)) => return Err(Error::new(ErrorKind::BindsAfterRest, ctx, loc)),
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

    pub fn r#macro(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;

        let MalVal::Fn(mut f) = value.value else {
            return Err(Error::new(
                ErrorKind::UnexpectedType(MalVal::FN, value.type_name()),
                ctx,
                value.loc,
            ));
        };

        f.is_macro = true;

        Ok(MalVal::Fn(f).with_loc(loc))
    }
}

pub fn fs(data: &mut Env) {
    data.set_fn(loc!(), "file", fs::file);
    data.set_fn(loc!(), "load-mal", fs::load_mal);
    data.set_fn(loc!(), "read-file", fs::read_file);
}

mod fs {
    use std::{fs, path::PathBuf};

    use crate::{
        env::Env,
        list,
        ns::{self, to_str},
        reader::{self, Location},
        types::{CallContext, DirContext, List, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::take_exact;

    pub fn load_mal(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let dir = take_dir_context(ctx, &loc)?;
        let (rel_path, abs_path) = take_path(ctx, dir, (args, &loc))?;

        let input = match fs::read_to_string(&abs_path) {
            Ok(input) => input,
            Err(e) => return Err(Error::new(ErrorKind::Io(e), ctx, loc)),
        };

        let file_loc = Location::file(rel_path);

        match reader::parse(file_loc.clone(), &input) {
            Ok(vals) => {
                let mut vals = List::from_vec(vals);
                vals.push_front(MalVal::Sym("do".into()).with_loc(file_loc.clone()));

                let ctx = CallContext::new(dir.root(), abs_path);

                let mut env = {
                    let mut data = Env::empty();
                    ns::std(&mut data);
                    data
                };

                env.eval(&ctx, MalVal::List(vals).with_loc(file_loc))
            }
            Err(e) => Err(Error::new(ErrorKind::Parse(e.to_string()), ctx, loc)),
        }
    }

    pub fn read_file(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let dir = take_dir_context(ctx, &loc)?;
        let (_, path) = take_path(ctx, dir, (args, &loc))?;

        match fs::read_to_string(path) {
            Ok(s) => Ok(MalVal::Str(s).with_loc(loc)),
            Err(e) => todo!("{e}"),
        }
    }

    fn take_dir_context<'ctx>(
        ctx: &'ctx CallContext,
        loc: &Location,
    ) -> Result<&'ctx DirContext, Error> {
        let Some(dir) = ctx.dir_context() else {
            return Err(Error::new(ErrorKind::DeniedFsAccess, ctx, loc.clone()));
        };

        Ok(dir)
    }

    fn take_path(
        ctx: &CallContext,
        dir: &DirContext,
        (args, loc): (List, &Location),
    ) -> Result<(String, PathBuf), Error> {
        let [value] = take_exact(ctx, loc, args)?;
        let rel_path = to_str(ctx, value)?;

        let Some(abs_path) = dir.canonicalize(&rel_path) else {
            return Err(Error::new(
                ErrorKind::InvalidPath(rel_path.into()),
                ctx,
                loc.clone(),
            ));
        };

        Ok((rel_path, abs_path))
    }

    pub fn file(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [] = take_exact(ctx, &loc, args)?;

        match ctx.file() {
            Some(path) => Ok(MalVal::Str(path.to_string_lossy().into()).with_loc(loc)),
            None => Ok(list!().with_loc(loc)),
        }
    }
}

pub fn fmt(data: &mut Env) {
    data.set_fn(loc!(), "prin", fmt::prin);
    data.set_fn(loc!(), "print", fmt::print);
}

mod fmt {
    use std::fmt;

    use crate::{
        reader::Location,
        types::{CallContext, List, MalVal},
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

    pub fn prin(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;
        print!("{}", PrettyPrint(&value.value));
        Ok(value)
    }

    pub fn print(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;
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
        types::{CallContext, List, MalData, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{all, all_reduce, take_exact};

    pub fn eq(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all_reduce(loc, args, eq2)
    }

    pub fn not(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;
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

    pub fn lt(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        cmp_num(loc, args, |ord| matches!(ord, Ordering::Less))
    }

    pub fn lte(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        cmp_num(loc, args, |ord| {
            matches!(ord, Ordering::Less | Ordering::Equal)
        })
    }

    pub fn gt(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        cmp_num(loc, args, |ord| matches!(ord, Ordering::Greater))
    }

    pub fn gte(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
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

    pub fn is_list(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all(loc, args, |value| {
            Ok(matches!(value.value, MalVal::List(_)))
        })
    }

    pub fn is_empty(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all(loc, args, |value| {
            Ok(match value.value {
                MalVal::List(list) => list.is_empty(),
                MalVal::Vector(vec) => vec.is_empty(),
                MalVal::Map(map) => map.is_empty(),
                _ => {
                    return {
                        let kind = ErrorKind::InvalidOperation1("empty?", value.type_name());
                        Err(Error::new(kind, ctx, value.loc))
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
        types::{CallContext, List, MalKey, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{take_exact, to_list_like};

    pub fn nth(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value, index] = take_exact(ctx, &loc, args)?;

        let mut value = to_list_like(ctx, value)?;

        let MalVal::Int(index) = index.value else {
            return Err(Error::new(
                ErrorKind::UnexpectedType(MalVal::INT, index.type_name()),
                ctx,
                index.loc,
            ));
        };

        if index < 0 || index >= value.len() as i64 {
            return Err(Error::new(
                ErrorKind::IndexOutOfRange(index, value.len()),
                ctx,
                loc,
            ));
        }

        Ok(value.swap_remove(index as usize))
    }

    pub fn first(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;
        let mut value = to_list_like(ctx, value)?.to_list();
        let Some(value) = value.pop_front() else {
            return Err(Error::new(ErrorKind::FirstOfEmptyList, ctx, loc));
        };
        Ok(value)
    }

    pub fn rest(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;
        let mut ls = to_list_like(ctx, value)?.to_list();
        ls.pop_front();
        Ok(MalVal::List(ls).with_loc(loc))
    }

    pub fn cons(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [el, value] = take_exact(ctx, &loc, args)?;

        let mut list = to_list_like(ctx, value)?.to_list();

        list.push_front(el);

        Ok(MalVal::List(list).with_loc(loc))
    }

    pub fn concat(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let mut list = List::new();

        for value in args.into_rev() {
            for el in to_list_like(ctx, value)?.into_iter().rev() {
                list.push_front(el);
            }
        }

        Ok(MalVal::List(list).with_loc(loc))
    }

    pub fn hash_map(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let mut args = args.into_iter();
        let mut map = IndexMap::with_capacity(args.len() / 2);

        while let Some(key) = args.next() {
            let Some(value) = args.next() else {
                return Err(Error::new(ErrorKind::UnevenArguments("hash-map"), ctx, loc));
            };

            let key = match MalKey::from_value(key.value) {
                Ok(key) => key,
                Err(key) => {
                    return Err(Error::new(
                        ErrorKind::InvalidMapKey(key.type_name()),
                        ctx,
                        loc,
                    ))
                }
            };

            map.insert(key, value);
        }

        Ok(MalVal::Map(map).with_loc(loc))
    }

    pub fn list(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        Ok(MalVal::List(args).with_loc(loc))
    }

    pub fn vec(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        Ok(MalVal::Vector(args.into_vec()).with_loc(loc))
    }

    pub fn count(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;

        Ok(MalVal::Int(match value.value {
            MalVal::List(list) => list.len(),
            MalVal::Vector(vec) => vec.len(),
            MalVal::Map(map) => map.len(),
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidOperation1("count", value.type_name()),
                    ctx,
                    loc,
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
        types::{CallContext, List, MalData, MalVal},
        Error, MalRet,
    };

    use super::{op_error, reduce};

    pub fn add(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        reduce(ctx, loc, args, add2)
    }

    fn add2(ctx: &CallContext, fst: MalData, snd: MalData) -> Result<MalVal, Error> {
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
            vals => Err(op_error(ctx, fst.loc, "+", vals)),
        }
    }

    pub fn sub(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        reduce(ctx, loc, args, |ctx, fst, snd| {
            match (fst.value, snd.value) {
                (MalVal::Int(int), MalVal::Float(float))
                | (MalVal::Float(float), MalVal::Int(int)) => {
                    Ok(MalVal::Float(float - (int as f64)))
                }
                (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a - b)),
                (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a - b)),
                vals => Err(op_error(ctx, fst.loc, "-", vals)),
            }
        })
    }

    pub fn mul(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        reduce(ctx, loc, args, |ctx, fst, snd| {
            match (fst.value, snd.value) {
                (MalVal::Int(int), MalVal::Float(float))
                | (MalVal::Float(float), MalVal::Int(int)) => {
                    Ok(MalVal::Float(float * (int as f64)))
                }
                (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a * b)),
                (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a * b)),
                vals => Err(op_error(ctx, fst.loc, "*", vals)),
            }
        })
    }

    pub fn div(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        reduce(ctx, loc, args, |ctx, fst, snd| {
            match (fst.value, snd.value) {
                (MalVal::Int(int), MalVal::Float(float))
                | (MalVal::Float(float), MalVal::Int(int)) => {
                    Ok(MalVal::Float(float / (int as f64)))
                }
                (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a / b)),
                (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a / b)),
                vals => Err(op_error(ctx, fst.loc, "/", vals)),
            }
        })
    }
}

fn op_error(
    ctx: &CallContext,
    loc: Location,
    op: &'static str,
    (fst, snd): (MalVal, MalVal),
) -> Error {
    Error::new(
        ErrorKind::InvalidOperation {
            op,
            fst: fst.type_name(),
            snd: snd.type_name(),
        },
        ctx,
        loc,
    )
}

fn to_str(ctx: &CallContext, value: MalData) -> Result<String, Error> {
    let MalVal::Str(key) = value.value else {
        return {
            Err(Error::new(
                ErrorKind::UnexpectedType(MalVal::STR, value.type_name()),
                ctx,
                value.loc,
            ))
        };
    };

    Ok(key)
}

fn to_sym(ctx: &CallContext, value: MalData) -> Result<String, Error> {
    let MalVal::Sym(key) = value.value else {
        return {
            Err(Error::new(
                ErrorKind::UnexpectedType(MalVal::SYM, value.type_name()),
                ctx,
                value.loc,
            ))
        };
    };

    Ok(key)
}

fn to_list_like(ctx: &CallContext, val: MalData) -> Result<ListLike, Error> {
    match val.value {
        MalVal::List(list) => Ok(ListLike::List(list)),
        MalVal::Vector(vec) => Ok(ListLike::Vector(vec)),
        _ => Err(Error::new(
            ErrorKind::UnexpectedType(MalVal::LIST_LIKE, val.type_name()),
            ctx,
            val.loc,
        )),
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
    ctx: &CallContext,
    loc: Location,
    args: List,
    join: impl Fn(&CallContext, MalData, MalData) -> Result<MalVal, Error>,
) -> MalRet {
    let mut iter = args.into_iter();
    let Some(mut accum) = iter.next() else {
        return Ok(list!().with_loc(loc));
    };

    for value in iter {
        accum = join(ctx, accum, value)?.with_loc(loc.clone());
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
    ctx: &CallContext,
    loc: &Location,
    mut list: List,
) -> Result<([MalData; N], List), Error> {
    if list.len() < N {
        return Err(Error::new(
            ErrorKind::AtleastArityMismatch(N, list.len()),
            ctx,
            loc.clone(),
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
    ctx: &CallContext,
    loc: &Location,
    args: List,
) -> Result<[MalData; N], Error> {
    match args.into_array::<N>() {
        Ok(arr) => Ok(arr),
        Err(list) => Err(Error::new(
            ErrorKind::ArityMismatch(N, list.len()),
            ctx,
            loc.clone(),
        )),
    }
}
