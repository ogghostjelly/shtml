use indexmap::IndexMap;

use crate::{
    env::Env,
    reader::{self, Location},
    types::{CallContext, List, MalData, MalFn, MalKey, MalVal},
    Error, ErrorKind, MalRet,
};

pub fn std(data: &mut Env) {
    sform(data);
    math(data);
    ds(data);
    cmp(data);
    fmt(data);
    env(data);
    fs(data);
    string(data);
    std_mal(data);
}

fn std_mal(data: &mut Env) {
    let ast = reader::parse_mal_str(Location::file("std.mal"), include_str!("std.mal"))
        .expect("parsing 'std.mal' should never fail");

    for value in ast {
        _ = data
            .eval(&CallContext::std(), value)
            .expect("evaluating 'std.mal' should never fail")
    }
}

pub fn sform(data: &mut Env) {
    data.set_special("let*", sform::r#let);
    data.set_special("def!", sform::def);
    data.set_special("do", sform::r#do);
    data.set_special("if", sform::r#if);
    data.set_special("fn*", sform::r#fn);
    data.set_fn("macro", sform::r#macro);

    data.set_special("quote", sform::quote);
    data.set_special("quasiquote", sform::quasiquote);
}

mod sform {
    use crate::{
        env::{Env, TcoRet, TcoVal},
        ns::take_atleast,
        reader::Location,
        types::{CallContext, List, MalFn, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{take_exact, to_fn, to_list_like, to_sym};

    pub fn quote(ctx: &CallContext, _: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [value] = take_exact(ctx, &loc, args)?;
        Ok(TcoVal::Val(value))
    }

    pub fn quasiquote(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [args] = take_exact(ctx, &loc, args)?;

        let MalVal::List(args) = args.value.as_ref() else {
            return Ok(TcoVal::Val(args));
        };

        quasiquote_inner(ctx, env, (args.clone(), loc))
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
            let MalVal::List(value) = data.value.as_ref() else {
                new_list.push_front(data);
                continue;
            };

            match try_take1_sym(value.clone(), "splice-unquote") {
                Ok(args) => {
                    let ctx = ctx.new_frame(("splice-unquote".into(), data.loc.clone()));
                    let [value] = take_exact(&ctx, &data.loc, args)?;
                    let value = env.eval(&ctx, value)?;
                    for el in to_list_like(&ctx, value)?.into_iter().rev() {
                        new_list.push_front(el);
                    }
                }
                Err(value) => new_list.push_front({
                    let value = quasiquote_inner(ctx, env, (value, data.loc.clone()))?;
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

        if let MalVal::Sym(sym) = first.value.as_ref() {
            if sym == s {
                return Ok(args);
            } else {
                args.push_front(MalVal::Sym(sym.clone()).with_loc(first.loc.clone()));
            }
        } else {
            args.push_front(first);
        }

        Err(args)
    }

    pub fn r#let(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [bindings, body] = take_exact(ctx, &loc, args)?;
        let mut bindings = to_list_like(ctx, bindings)?.into_iter();

        let mut env = Env::inner("let*".into(), env);

        while let Some(key) = bindings.next() {
            let Some(value) = bindings.next() else {
                return Err(Error::new(
                    ErrorKind::UnevenArguments("let*"),
                    ctx,
                    key.loc.clone(),
                ));
            };

            let key = to_sym(ctx, &key)?;

            let value = env.eval(ctx, value)?;
            env.set(key, value.value);
        }

        Ok(TcoVal::Val(env.eval(ctx, body)?))
    }

    pub fn def(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [key, value] = take_exact(ctx, &loc, args)?;
        let key = to_sym(ctx, &key)?;
        let mut value = env.eval(ctx, value)?.value.as_ref().clone();

        if let MalVal::Fn(MalFn { name, .. }) = &mut value {
            *name = Some(key.clone());
        }

        env.set(key, value.into());

        Ok(TcoVal::Val(MalVal::Nil.with_loc(loc)))
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
            None => Ok(TcoVal::Val(MalVal::Nil.with_loc(loc))),
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
        let mut bind_rest: Option<Option<String>> = None;

        for value in to_list_like(ctx, bindings)?.into_iter() {
            let loc = value.loc.clone();
            let sym = to_sym(ctx, &value)?;

            match &bind_rest {
                Some(None) => bind_rest = Some(Some(sym.to_string())),
                Some(Some(_)) => return Err(Error::new(ErrorKind::BindsAfterRest, ctx, loc)),
                None => {
                    if sym == "&" {
                        bind_rest = Some(None);
                    } else {
                        binds.push(sym.to_string());
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
                body: (first, rest),
            })
            .with_loc(loc),
        ))
    }

    pub fn r#macro(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;
        let mut f = to_fn(ctx, &value)?.clone();
        f.is_macro = true;
        Ok(MalVal::Fn(f).with_loc(loc))
    }
}

pub fn fs(data: &mut Env) {
    data.set_fn("file", fs::file);
    data.set_fn("load-mal", fs::load_mal);
    data.set_fn("load-shtml", fs::load_shtml);
    data.set_fn("read-file", fs::read_file);
    data.set_fn("read-dir", fs::read_dir);
}

mod fs {
    use std::{fs, path::PathBuf};

    use crate::{
        load,
        ns::to_str,
        reader::Location,
        types::{CallContext, DirContext, List, MalData, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{take_exact, to_env};

    pub fn load_shtml(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let dir = take_dir_context(ctx, &loc)?;
        let [env, path] = take_exact(ctx, &loc, args)?;

        let mut env = to_env(ctx, &env)?.clone();
        let (rel_path, abs_path) = to_path(ctx, dir, path)?;

        let value = conv_err(load::shtml(ctx, &mut env, &rel_path, abs_path), ctx, &loc)?;

        Ok(MalVal::Str(value).with_loc(loc))
    }

    pub fn load_mal(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let dir = take_dir_context(ctx, &loc)?;
        let [env, path] = take_exact(ctx, &loc, args)?;

        let mut env = to_env(ctx, &env)?.clone();
        let (rel_path, abs_path) = to_path(ctx, dir, path)?;

        conv_err(load::mal(ctx, &mut env, &rel_path, abs_path), ctx, &loc)
    }

    fn conv_err<T>(
        res: Result<T, load::Error>,
        ctx: &CallContext,
        loc: &Location,
    ) -> Result<T, Error> {
        match res {
            Ok(val) => Ok(val),
            Err(e) => match e {
                load::Error::Io(_, e) => Err(Error::new(ErrorKind::Io(e), ctx, loc.clone())),
                load::Error::Parse(e) => Err(Error::new(ErrorKind::Parse(e), ctx, loc.clone())),
                load::Error::Shtml(_, e) => Err(*e),
                load::Error::CannotEmbed(data) => {
                    let loc = data.loc.clone();
                    Err(Error::new(ErrorKind::CannotEmbed(data), ctx, loc))
                }
                load::Error::Fmt(_) => unreachable!("fmt is infallible"),
            },
        }
    }

    pub fn read_file(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let dir = take_dir_context(ctx, &loc)?;
        let [path] = take_exact(ctx, &loc, args)?;
        let (_, path) = to_path(ctx, dir, path)?;

        match fs::read_to_string(path) {
            Ok(s) => Ok(MalVal::Str(s).with_loc(loc)),
            Err(e) => Err(Error::new(ErrorKind::Io(e), ctx, loc)),
        }
    }

    pub fn read_dir(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let dir = take_dir_context(ctx, &loc)?;
        let [path] = take_exact(ctx, &loc, args)?;
        let (_, path) = to_path(ctx, dir, path)?;

        let dir = match fs::read_dir(&path) {
            Ok(dir) => dir,
            Err(e) => return Err(Error::new(ErrorKind::Io(e), ctx, loc)),
        };

        let mut paths = vec![];

        for entry in dir {
            let entry = match entry {
                Ok(entry) => entry,
                Err(e) => return Err(Error::new(ErrorKind::Io(e), ctx, loc)),
            };

            let path = entry
                .path()
                .strip_prefix(&path)
                .expect("DirEntry should have the source directory as a prefix")
                .to_string_lossy()
                .to_string();

            paths.push(path);
        }

        paths.sort();
        let paths = paths
            .into_iter()
            .map(|value| MalVal::Str(value).with_loc(loc.clone()))
            .collect();

        Ok(MalVal::Vector(paths).with_loc(loc))
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

    fn to_path(
        ctx: &CallContext,
        dir: &DirContext,
        path: MalData,
    ) -> Result<(String, PathBuf), Error> {
        let loc = path.loc.clone();
        let rel_path = to_str(ctx, &path)?;

        let Some(abs_path) = dir.canonicalize(rel_path) else {
            return Err(Error::new(
                ErrorKind::InvalidPath(rel_path.into()),
                ctx,
                loc,
            ));
        };

        Ok((rel_path.clone(), abs_path))
    }

    pub fn file(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [] = take_exact(ctx, &loc, args)?;

        match ctx.file() {
            Some(path) => Ok(MalVal::Str(path.to_string()).with_loc(loc)),
            None => Ok(MalVal::Nil.with_loc(loc)),
        }
    }
}

pub fn fmt(data: &mut Env) {
    data.set_fn("prin", fmt::prin);
    data.set_fn("print", fmt::print);
    data.set_fn("dbg", fmt::dbg);
}

mod fmt {
    use std::fmt;

    use crate::{
        ns::take_atleast,
        reader::Location,
        types::{CallContext, List, MalVal},
        MalRet,
    };

    struct PrettyPrint<'a>(&'a MalVal);

    impl fmt::Display for PrettyPrint<'_> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match &self.0 {
                MalVal::Str(s) => s.fmt(f),
                _ => self.0.fmt(f),
            }
        }
    }

    pub fn prin(_ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let mut iter = args.iter();
        if let Some(value) = iter.next() {
            print!("{}", PrettyPrint(value.value.as_ref()));
        }
        for value in iter {
            print!(" {}", PrettyPrint(value.value.as_ref()));
        }
        Ok(MalVal::Nil.with_loc(loc))
    }

    pub fn print(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let value = prin(ctx, (args, loc))?;
        println!();
        Ok(value)
    }

    pub fn dbg(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let ([value], rest) = take_atleast(ctx, &loc, args)?;

        print!("dbg:");
        for value in rest {
            print!(" {}", PrettyPrint(value.value.as_ref()));
        }
        println!(": {} at {}", value.value, loc);

        Ok(value)
    }
}

pub fn cmp(data: &mut Env) {
    data.set_fn("=", cmp::eq);
    data.set_fn("!=", cmp::neq);
    data.set_fn("lt", cmp::lt);
    data.set_fn("lt=", cmp::lte);
    data.set_fn("gt", cmp::gt);
    data.set_fn("gt=", cmp::gte);

    data.set_fn("not", cmp::not);
    data.set_special("and", cmp::and);
    data.set_special("or", cmp::or);

    data.set_fn("list?", cmp::is_list);
    data.set_fn("empty?", cmp::is_empty);
    data.set_fn("string?", cmp::is_string);
    data.set_fn("vec?", cmp::is_vec);
    data.set_fn("hash-map?", cmp::is_hash_map);
    data.set_fn("sym?", cmp::is_sym);
    data.set_fn("fn?", cmp::is_fn);
    data.set_fn("even?", cmp::is_even);
    data.set_fn("odd?", cmp::is_odd);
}

mod cmp {
    use std::cmp::Ordering;

    use crate::{
        env::{Env, TcoRet, TcoVal},
        reader::Location,
        types::{CallContext, List, MalData, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{all, all_reduce, take_exact};

    pub fn eq(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all_reduce(loc, args, eq2)
    }

    pub fn neq(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all_reduce(loc, args, |a, b| Ok(!eq2(a, b)?))
    }

    pub fn not(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;
        Ok(MalVal::Bool(!value.is_true()).with_loc(loc))
    }

    pub fn and(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [a, b] = take_exact(ctx, &loc, args)?;
        if !env.eval(ctx, a)?.is_true() {
            return Ok(TcoVal::Val(MalVal::Bool(false).with_loc(loc)));
        }
        if !env.eval(ctx, b)?.is_true() {
            return Ok(TcoVal::Val(MalVal::Bool(false).with_loc(loc)));
        }
        Ok(TcoVal::Val(MalVal::Bool(true).with_loc(loc)))
    }

    pub fn or(ctx: &CallContext, env: &mut Env, (args, loc): (List, Location)) -> TcoRet {
        let [a, b] = take_exact(ctx, &loc, args)?;
        let a = env.eval(ctx, a)?;
        if a.is_true() {
            return Ok(TcoVal::Val(a));
        }
        let b = env.eval(ctx, b)?;
        if b.is_true() {
            return Ok(TcoVal::Val(b));
        }
        Ok(TcoVal::Val(MalVal::Bool(true).with_loc(loc)))
    }

    pub fn eq2(fst: &MalData, snd: &MalData) -> Result<bool, Error> {
        Ok(match (fst.value.as_ref(), snd.value.as_ref()) {
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
            let ord = match (fst.value.as_ref(), snd.value.as_ref()) {
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
            Ok(matches!(value.value.as_ref(), MalVal::List(_)))
        })
    }

    pub fn is_empty(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all(loc, args, |value| {
            Ok(match value.value.as_ref() {
                MalVal::List(list) => list.is_empty(),
                MalVal::Vector(vec) => vec.is_empty(),
                MalVal::Map(map) => map.is_empty(),
                _ => {
                    let kind = ErrorKind::InvalidOperation1("empty?", value.type_name());
                    return Err(Error::new(kind, ctx, value.loc.clone()));
                }
            })
        })
    }

    pub fn is_string(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all(loc, args, |value| {
            Ok(matches!(value.value.as_ref(), MalVal::Str(_)))
        })
    }

    pub fn is_vec(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all(loc, args, |value| {
            Ok(matches!(value.value.as_ref(), MalVal::Vector(_)))
        })
    }

    pub fn is_hash_map(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all(loc, args, |value| {
            Ok(matches!(value.value.as_ref(), MalVal::Map(_)))
        })
    }

    pub fn is_sym(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all(loc, args, |value| {
            Ok(matches!(value.value.as_ref(), MalVal::Sym(_)))
        })
    }

    pub fn is_fn(_: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all(loc, args, |value| {
            Ok(matches!(
                value.value.as_ref(),
                MalVal::Fn(_) | MalVal::BuiltinFn(_, _) | MalVal::Special(_, _)
            ))
        })
    }

    pub fn is_even(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all(loc, args, |value| match value.value.as_ref() {
            MalVal::Int(x) => Ok(x % 2 == 0),
            _ => {
                let kind = ErrorKind::InvalidOperation1("even?", value.type_name());
                Err(Error::new(kind, ctx, value.loc.clone()))
            }
        })
    }

    pub fn is_odd(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        all(loc, args, |value| match value.value.as_ref() {
            MalVal::Int(x) => Ok(x % 2 == 1),
            _ => {
                let kind = ErrorKind::InvalidOperation1("odd?", value.type_name());
                Err(Error::new(kind, ctx, value.loc.clone()))
            }
        })
    }
}

pub fn env(data: &mut Env) {
    data.set_fn("new-env", env::new_env);
    data.set_fn("env/set", env::set);
    data.set_fn("env/sets", env::sets);
    data.set_fn("env/get", env::get);
}

mod env {
    use std::rc::Rc;

    use crate::{
        reader::Location,
        types::{CallContext, List, MalKey, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{take_exact, to_env, to_hash_map, to_sym};

    pub fn new_env(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [] = take_exact(ctx, &loc, args)?;
        Ok(MalVal::Env(ctx.new_env()).with_loc(loc))
    }

    pub fn set(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [env, key, value] = take_exact(ctx, &loc, args)?;
        let mut env = to_env(ctx, &env)?.clone();
        let key = to_sym(ctx, &key)?.clone();

        env.set(key, value.value);

        Ok(MalVal::Env(env).with_loc(loc))
    }

    pub fn sets(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [env, kvps] = take_exact(ctx, &loc, args)?;
        let mut env = to_env(ctx, &env)?.clone();
        let kvps = to_hash_map(ctx, &kvps)?;

        for (key, value) in kvps {
            let MalKey::Sym(key) = key else {
                return Err(Error::new(
                    ErrorKind::UnexpectedType(MalVal::SYM, key.type_name()),
                    ctx,
                    loc,
                ));
            };

            env.set(key.clone(), Rc::clone(&value.value));
        }

        Ok(MalVal::Env(env).with_loc(loc))
    }

    pub fn get(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [env, key] = take_exact(ctx, &loc, args)?;
        let env = to_env(ctx, &env)?;
        let key = to_sym(ctx, &key)?;

        let value = env.get(ctx, &loc, key)?;
        Ok(value.clone())
    }
}

pub fn ds(data: &mut Env) {
    data.set_fn("hash-map", ds::hash_map);
    data.set_fn("list", ds::list);
    data.set_fn("vec", ds::vec);
    data.set_fn("sym", ds::sym);

    data.set_fn("cons", ds::cons);
    data.set_fn("concat", ds::concat);

    data.set_fn("fst", ds::fst);
    data.set_fn("snd", ds::snd);
    data.set_fn("rest", ds::rest);
    data.set_fn("nth", ds::nth);

    data.set_fn("map/take", ds::map_take);

    data.set_fn("len", ds::len);
}

pub fn apply_map(
    map: &IndexMap<MalKey, MalData>,
    ctx: &CallContext,
    (args, loc): (List, Location),
) -> MalRet {
    let [value] = take_exact(ctx, &loc, args)?;
    let key = to_key(ctx, value)?;
    match map.get(&key) {
        Some(value) => Ok(value.clone()),
        None => Err(Error::new(
            ErrorKind::MapKeyNotFound(Box::new(key.into_value())),
            ctx,
            loc,
        )),
    }
}

mod ds {
    use indexmap::IndexMap;

    use crate::{
        list,
        reader::Location,
        types::{CallContext, List, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{take_exact, to_hash_map, to_key, to_list_like};

    pub fn nth(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value, index] = take_exact(ctx, &loc, args)?;

        let mut value = to_list_like(ctx, value)?;

        let MalVal::Int(index) = *index.value else {
            return Err(Error::new(
                ErrorKind::UnexpectedType(MalVal::INT, index.type_name()),
                ctx,
                index.loc.clone(),
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

    pub fn fst(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;
        let mut value = to_list_like(ctx, value)?.to_list();
        let Some(value) = value.pop_front() else {
            return Ok(MalVal::Nil.with_loc(loc));
        };
        Ok(value)
    }

    pub fn snd(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;
        let mut value = to_list_like(ctx, value)?.to_list();
        let Some(_) = value.pop_front() else {
            return Ok(MalVal::Nil.with_loc(loc));
        };
        let Some(value) = value.pop_front() else {
            return Ok(MalVal::Nil.with_loc(loc));
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

            let key = to_key(ctx, key)?;

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

    pub fn sym(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;
        let str = match value.value.as_ref() {
            MalVal::Sym(value) | MalVal::Str(value) | MalVal::Kwd(value) => value,
            _ => {
                return Err(Error::new(
                    ErrorKind::UnexpectedType(MalVal::STR, value.type_name()),
                    ctx,
                    value.loc.clone(),
                ))
            }
        };
        Ok(MalVal::Sym(str.to_string()).with_loc(loc))
    }

    pub fn map_take(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [map_dat, key] = take_exact(ctx, &loc, args)?;
        let mut map = to_hash_map(ctx, &map_dat)?.clone();
        let key = to_key(ctx, key)?;

        let Some(value) = map.shift_remove(&key) else {
            return Err(Error::new(
                ErrorKind::MapKeyNotFound(Box::new(key.into_value())),
                ctx,
                loc,
            ));
        };

        Ok(list!(value, MalVal::Map(map).with_loc(map_dat.loc.clone())).with_loc(loc))
    }

    pub fn len(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value] = take_exact(ctx, &loc, args)?;

        Ok(MalVal::Int(match value.value.as_ref() {
            MalVal::List(list) => list.len(),
            MalVal::Vector(vec) => vec.len(),
            MalVal::Map(map) => map.len(),
            MalVal::Str(s) => s.len(),
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidOperation1("len", value.type_name()),
                    ctx,
                    loc,
                ))
            }
        } as i64)
        .with_loc(loc))
    }
}

pub fn string(data: &mut Env) {
    data.set_fn("str", string::str);
    data.set_fn("string/rsplit", string::rsplit_once);
    data.set_fn("string/split", string::split_once);
    data.set_fn("string/ends-with", string::ends_with);
    data.set_fn("string/starts-with", string::starts_with);
}

mod string {
    use std::fmt::Write;

    use crate::{
        list,
        reader::Location,
        types::{CallContext, List, MalVal},
        Error, ErrorKind, MalRet,
    };

    use super::{take_exact, to_str};

    pub fn str(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let mut s = String::new();

        for value in args {
            match value.value.as_ref() {
                MalVal::Sym(value) => write!(s, "{value}"),
                MalVal::Str(value) => write!(s, "{value}"),
                MalVal::Int(value) => write!(s, "{value}"),
                MalVal::Float(value) => write!(s, "{value}"),
                MalVal::Bool(value) => write!(s, "{value}"),
                MalVal::Html(html) => write!(s, "{html}"),
                _ => {
                    return Err(Error::new(
                        ErrorKind::InvalidOperation1("str", value.type_name()),
                        ctx,
                        loc,
                    ))
                }
            }
            .expect("write! fmt is infallible")
        }

        Ok(MalVal::Str(s).with_loc(loc))
    }

    macro_rules! make_split_once {
        ( $name:ident ) => {
            pub fn $name(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
                let [value, pat] = take_exact(ctx, &loc, args)?;
                let value = to_str(ctx, &value)?;
                let pat = to_str(ctx, &pat)?;

                match value.$name(pat) {
                    Some((l, r)) => Ok(list!(
                        MalVal::Str(l.to_string()).with_loc(loc.clone()),
                        MalVal::Str(r.to_string()).with_loc(loc.clone())
                    )
                    .with_loc(loc)),
                    None => Ok(MalVal::Nil.with_loc(loc)),
                }
            }
        };
    }

    make_split_once!(rsplit_once);
    make_split_once!(split_once);

    pub fn ends_with(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value, pat] = take_exact(ctx, &loc, args)?;
        let value = to_str(ctx, &value)?;
        let pat = to_str(ctx, &pat)?;
        Ok(MalVal::Bool(value.ends_with(pat)).with_loc(loc))
    }

    pub fn starts_with(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        let [value, pat] = take_exact(ctx, &loc, args)?;
        let value = to_str(ctx, &value)?;
        let pat = to_str(ctx, &pat)?;
        Ok(MalVal::Bool(value.starts_with(pat)).with_loc(loc))
    }
}

pub fn math(data: &mut Env) {
    data.set_fn("+", math::add);
    data.set_fn("-", math::sub);
    data.set_fn("*", math::mul);
    data.set_fn("/", math::div);
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
        match (fst.value.as_ref(), snd.value.as_ref()) {
            (MalVal::Int(int), MalVal::Float(float)) | (MalVal::Float(float), MalVal::Int(int)) => {
                Ok(MalVal::Float(float + (*int as f64)))
            }
            (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a + b)),
            (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a + b)),
            (MalVal::Str(a), MalVal::Str(b)) => Ok(MalVal::Str(format!("{a}{b}"))),
            (MalVal::List(a), MalVal::List(b)) => {
                let (mut a, mut b) = (a.clone(), b.clone());
                a.append(&mut b);
                Ok(MalVal::List(a))
            }
            (MalVal::List(a), MalVal::Vector(b)) => {
                let mut a = a.clone();
                a.append(&mut List::from_vec(b.to_vec()));
                Ok(MalVal::List(a))
            }
            (MalVal::Vector(a), MalVal::Vector(b)) => {
                let (mut a, mut b) = (a.clone(), b.clone());
                a.append(&mut b);
                Ok(MalVal::Vector(a))
            }
            (MalVal::Vector(a), MalVal::List(b)) => {
                let mut a = a.clone();
                a.append(&mut b.clone().into_vec());
                Ok(MalVal::Vector(a))
            }
            vals => Err(op_error(ctx, fst.loc.clone(), "+", vals)),
        }
    }

    pub fn sub(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        reduce(ctx, loc, args, |ctx, fst, snd| {
            match (fst.value.as_ref(), snd.value.as_ref()) {
                (MalVal::Int(int), MalVal::Float(float))
                | (MalVal::Float(float), MalVal::Int(int)) => {
                    Ok(MalVal::Float(float - (*int as f64)))
                }
                (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a - b)),
                (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a - b)),
                vals => Err(op_error(ctx, fst.loc.clone(), "-", vals)),
            }
        })
    }

    pub fn mul(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        reduce(ctx, loc, args, |ctx, fst, snd| {
            match (fst.value.as_ref(), snd.value.as_ref()) {
                (MalVal::Int(int), MalVal::Float(float))
                | (MalVal::Float(float), MalVal::Int(int)) => {
                    Ok(MalVal::Float(float * (*int as f64)))
                }
                (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a * b)),
                (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a * b)),
                vals => Err(op_error(ctx, fst.loc.clone(), "*", vals)),
            }
        })
    }

    pub fn div(ctx: &CallContext, (args, loc): (List, Location)) -> MalRet {
        reduce(ctx, loc, args, |ctx, fst, snd| {
            match (fst.value.as_ref(), snd.value.as_ref()) {
                (MalVal::Int(int), MalVal::Float(float))
                | (MalVal::Float(float), MalVal::Int(int)) => {
                    Ok(MalVal::Float(float / (*int as f64)))
                }
                (MalVal::Int(a), MalVal::Int(b)) => Ok(MalVal::Int(a / b)),
                (MalVal::Float(a), MalVal::Float(b)) => Ok(MalVal::Float(a / b)),
                vals => Err(op_error(ctx, fst.loc.clone(), "/", vals)),
            }
        })
    }
}

fn op_error(
    ctx: &CallContext,
    loc: Location,
    op: &'static str,
    (fst, snd): (&MalVal, &MalVal),
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

fn to_fn<'e>(ctx: &CallContext, value: &'e MalData) -> Result<&'e MalFn, Error> {
    let MalVal::Fn(f) = value.value.as_ref() else {
        return Err(Error::new(
            ErrorKind::UnexpectedType(MalVal::FN, value.type_name()),
            ctx,
            value.loc.clone(),
        ));
    };

    Ok(f)
}

fn to_hash_map<'e>(
    ctx: &CallContext,
    value: &'e MalData,
) -> Result<&'e IndexMap<MalKey, MalData>, Error> {
    let MalVal::Map(map) = value.value.as_ref() else {
        return Err(Error::new(
            ErrorKind::UnexpectedType(MalVal::HASH_MAP, value.type_name()),
            ctx,
            value.loc.clone(),
        ));
    };

    Ok(map)
}

fn to_env<'e>(ctx: &CallContext, value: &'e MalData) -> Result<&'e Env, Error> {
    let MalVal::Env(env) = value.value.as_ref() else {
        return Err(Error::new(
            ErrorKind::UnexpectedType(MalVal::ENV, value.type_name()),
            ctx,
            value.loc.clone(),
        ));
    };

    Ok(env)
}

fn to_key(ctx: &CallContext, value: MalData) -> Result<MalKey, Error> {
    match MalKey::from_value(value.value.as_ref()) {
        Some(key) => Ok(key),
        None => Err(Error::new(
            ErrorKind::InvalidMapKey(value.type_name()),
            ctx,
            value.loc.clone(),
        )),
    }
}

fn to_str<'e>(ctx: &CallContext, value: &'e MalData) -> Result<&'e String, Error> {
    let MalVal::Str(key) = value.value.as_ref() else {
        return Err(Error::new(
            ErrorKind::UnexpectedType(MalVal::STR, value.type_name()),
            ctx,
            value.loc.clone(),
        ));
    };

    Ok(key)
}

fn to_sym<'e>(ctx: &CallContext, value: &'e MalData) -> Result<&'e String, Error> {
    let MalVal::Sym(key) = value.value.as_ref() else {
        return Err(Error::new(
            ErrorKind::UnexpectedType(MalVal::SYM, value.type_name()),
            ctx,
            value.loc.clone(),
        ));
    };

    Ok(key)
}

fn to_list_like(ctx: &CallContext, value: MalData) -> Result<ListLike, Error> {
    match value.value.as_ref() {
        MalVal::List(list) => Ok(ListLike::List(list.clone())),
        MalVal::Vector(vec) => Ok(ListLike::Vector(vec.clone())),
        _ => Err(Error::new(
            ErrorKind::UnexpectedType(MalVal::LIST_LIKE, value.type_name()),
            ctx,
            value.loc.clone(),
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
        return Ok(MalVal::Nil.with_loc(loc));
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
        return Ok(MalVal::Nil.with_loc(loc));
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
