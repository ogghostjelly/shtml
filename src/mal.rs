use ogj_mal::{re, reader, Env, Error, MalVal};
use std::fmt::Write;

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
        output.push_str(&input[..tag + 1]);
        return Ok(&input[tag + 2..]);
    }

    let (text, lisp) = input.split_at(tag);
    let lisp = &lisp[1..];
    output.push_str(text);

    let mut _lisp = String::new();

    let (lisp, text) = if lisp.starts_with('(') {
        let end = find_closing_paren(lisp).ok_or(reader::Error::UnexpectedEof)?;
        let (lisp, text) = lisp.split_at(end + 1);
        _lisp = lisp.to_string();
        _lisp.insert(lisp.len() - 1, '\n');
        (_lisp.as_str(), text)
    } else {
        let end = lisp
            .find(|ch: char| ch.is_whitespace() || ch == '<')
            .map(|index| index - 1)
            .ok_or(reader::Error::UnexpectedEof)?;
        lisp.split_at(end + 1)
    };

    let value = ogj_mal::re(env, lisp)?;
    if !value.is_nil() && !matches!(&value, MalVal::List(ls) if ls.is_empty()) {
        _ = write!(output, "{value}")
    }

    Ok(text)
}

pub fn transform(mut input: &str) -> Result<String, Error> {
    let env = Env::default();
    env.apply_ns(ogj_mal::js::ns());
    re(&env, &format!("(do{}\n)", include_str!("lib.mal"))).expect("lib.mal should be valid mal");

    let mut output = String::with_capacity(input.len());
    while !input.is_empty() {
        input = transform_(&env, input, &mut output)?;
    }
    Ok(output)
}
