use ogj_mal::{env, func, re, reader, types::take_between_vec, Env, Error, MalVal};
use std::{fmt::Write, fs, path::Path, rc::Rc};

fn find_closing_paren(lisp: &str) -> Option<usize> {
    let mut count = 0;

    for (index, ch) in lisp.chars().enumerate() {
        match ch {
            '(' => count += 1,
            ')' => {
                count -= 1;
                if count <= 0 {
                    return Some(index);
                }
            }
            _ => continue,
        }
    }

    None
}

pub fn transform_<'i>(env: &Env, input: &'i str, output: &mut String) -> Result<&'i str, Error> {
    let Some(tag) = input.find('@') else {
        output.push_str(input);
        return Ok("");
    };

    if input[tag..].starts_with("@@") {
        output.push_str(&input[..=tag]);
        return Ok(&input[tag + 2..]);
    }

    let (text, lisp) = input.split_at(tag);
    let lisp = &lisp[1..];
    output.push_str(text);

    let mut lisp_tmp = String::new();

    let (lisp, text) = if lisp.starts_with('(') {
        let end = find_closing_paren(lisp).ok_or(reader::Error::UnexpectedEof)?;
        let (lisp, text) = lisp.split_at(end + 1);
        lisp_tmp.push_str(lisp);
        lisp_tmp.insert(lisp.len() - 1, '\n');
        (lisp_tmp.as_str(), text)
    } else {
        let end = lisp
            .find(|ch: char| ch.is_whitespace() || ch == '<')
            .map_or(lisp.len() - 1, |index| index - 1);
        lisp.split_at(end + 1)
    };

    let value = ogj_mal::re(env, lisp)?;
    if !value.is_nil() && !matches!(&value, MalVal::List(ls) if ls.is_empty()) {
        _ = write!(output, "{value}");
    }

    Ok(text)
}

pub struct Config<'a> {
    pub project_dir: &'a Path,
    pub file_path: &'a Path,
}

pub fn transform(env: &Env, config: &Config, mut input: &str) -> Result<String, Error> {
    env.set(
        "file-path".into(),
        MalVal::Str(config.file_path.to_string_lossy().into()),
    );
    env.set(
        "project-dir".into(),
        MalVal::Str(config.project_dir.to_string_lossy().into()),
    );

    env.apply_ns(ogj_mal::js::ns());
    env.apply_ns(&[(
        "shtml-include",
        func!(|env, args| {
            let mut args = take_between_vec(args, 1, 2)?;

            let file_path = args.swap_remove(0);
            let file_path = Path::new(file_path.to_str()?);

            let arg = if args.is_empty() {
                None
            } else {
                Some(Rc::clone(args.swap_remove(0).to_seq()?))
            };

            let project_dir = env.get("project-dir")?;
            let project_dir = Path::new(project_dir.to_str()?);

            let contents =
                fs::read_to_string(project_dir.join(file_path)).map_err(env::Error::Io)?;

            let env = Env::default();

            if let Some(arg) = arg {
                for chunk in arg.chunks_exact(2) {
                    let key = chunk[0].to_sym()?;
                    let value = &chunk[1];
                    env.set(key.to_owned(), value.clone());
                }
            }

            let ret = transform(
                &env,
                &Config {
                    project_dir,
                    file_path,
                },
                &contents,
            )?;
            Ok(MalVal::Str(ret))
        }),
    )]);
    setup_lib(env);

    let mut output = String::with_capacity(input.len());
    while !input.is_empty() {
        input = transform_(env, input, &mut output)?;
    }
    Ok(output)
}

fn setup_lib(env: &Env) {
    re(env, &format!("(do{}\n)", include_str!("lib.mal"))).expect("lib.mal should be valid mal");
}
