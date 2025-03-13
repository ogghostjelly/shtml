use std::fmt::{self, Write};

use ogj_mal::{re, Env};

fn find_open(input: &str) -> Option<usize> {
    let mut index = 0;
    while index < input.len() {
        if input[index..].starts_with("@(")
            | input[index..].starts_with("@@(")
            | input[index..].starts_with("@!(")
            | input[index..].starts_with("@@!(")
        {
            return Some(index);
        }
        index += 1;
    }
    None
}

fn find_expr_end(input: &str) -> Option<usize> {
    let mut index = 0;
    let mut paren_count: u32 = 0;

    while index < input.len() {
        let ch = input.as_bytes()[index];
        index += 1;

        match ch {
            b'(' => paren_count += 1,
            b')' => {
                paren_count = paren_count.checked_sub(1)?;
                if paren_count == 0 {
                    return Some(index);
                }
            }
            _ => {}
        }
    }

    None
}

fn split_expr(input: &str) -> (&str, &str, &str) {
    let Some(index) = find_open(input) else {
        return (input, "", "");
    };

    let (text, rest) = input.split_at(index);

    let (text, lisp, rest) = match find_expr_end(rest) {
        Some(index) => {
            let (lisp, rest) = rest.split_at(index);
            (text, lisp, rest)
        }
        None => (input, "", ""),
    };

    (text, lisp, rest)
}

struct Tokenizer<'a> {
    input: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }
}

#[derive(Default)]
struct Token<'a> {
    pub text: Option<&'a str>,
    pub escaped_lisp: Option<&'a str>,
    pub lisp: Option<&'a str>,
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.input.is_empty() {
            return None;
        }

        let (text, lisp, rest) = split_expr(self.input);
        self.input = rest;

        let mut token = Token::default();

        if !text.is_empty() {
            token.text = Some(text);
        }

        if !lisp.is_empty() {
            if lisp.starts_with("@@") {
                token.escaped_lisp = Some(lisp);
            } else {
                token.lisp = Some(lisp);
            }
        }

        Some(token)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Format(#[from] fmt::Error),
    #[error(transparent)]
    Mal(#[from] ogj_mal::Error),
}

/// Transforms text using the mal templating language.
///
/// # Panics
///
/// Panics if the built-in internal library `lib.mal` has invalid mal code.
/// This shouldn't happen under normal circumstances and you can safely ignore the possibility of a panic.
#[must_use]
pub fn transform(input: &str) -> String {
    let env = Env::default();
    env.apply_ns(ogj_mal::js::ns());
    re(&env, &format!("(do {}\n)", include_str!("lib.mal")))
        .expect("'lib.mal' should be valid mal");
    let mut output = String::new();
    match transform_inner(&env, input, &mut output) {
        Ok(()) => {}
        Err(e) => eprintln!("err: {e}"),
    }
    output
}

fn transform_inner(env: &Env, input: &str, mut output: impl Write) -> Result<(), Error> {
    for token in Tokenizer::new(input) {
        if let Some(text) = token.text {
            write!(output, "{text}")?;
        }

        if let Some(text) = token.escaped_lisp {
            let text = &text[1..];
            write!(output, "{text}")?;
        }

        if let Some(lisp) = token.lisp {
            let ignore_return = lisp.starts_with("@!");
            let offset_index = lisp.find('(').unwrap_or(0);
            let lisp = {
                let lisp = &lisp[offset_index..lisp.len()];
                let mut lisp = String::from(lisp);
                lisp.insert(lisp.len()-1, '\n');
                lisp
            };

            let text = re(env, &lisp)?;
            if !ignore_return {
                write!(output, "{text}")?;
            }
        }
    }
    Ok(())
}
