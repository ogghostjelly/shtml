use derive_more::Display;

use crate::list;
use crate::types::{List, MalVal};

pub fn parse_file(input: &str) -> Result<Vec<Element>, Error> {
    shtml(Span {
        data: input,
        line: 0,
        col: 0,
    })
}

pub fn parse(input: &str) -> Result<Option<MalVal>, Error> {
    let (rest, value) = value(Span {
        data: input,
        line: 0,
        col: 0,
    })?;

    if rest.data.len() != 0 {
        return Err(rest.err(ErrorKind::UnexpectedEof));
    }

    Ok(value)
}

fn shtml<'i>(input: Span<'i>) -> Result<Vec<Element>, Error<'i>> {
    let (mut input, mut elems) = (input, vec![]);

    loop {
        let (rest, text) = text(input);
        elems.push(Element::Text(text.data.to_string()));

        if rest.data.is_empty() {
            return Ok(elems);
        }

        let (rest, _) = Tag("@").parse(rest)?;
        let (rest, value) = value(rest)?;

        input = rest;

        if let Some(value) = value {
            elems.push(Element::Value(value));
        }
    }
}

#[derive(Debug, Display)]
pub enum Element {
    Text(String),
    #[display("@{_0}")]
    Value(MalVal),
}

fn text<'i>(input: Span<'i>) -> (Span<'i>, Span<'i>) {
    let mut chars = input.data.char_indices().peekable();
    let mut index = input.data.len();

    while let Some((i, ch)) = chars.next() {
        if ch == '@' {
            if let Some((_, '@')) = chars.peek() {
                _ = chars.next();
            } else {
                index = i;
            }
        }
    }

    input.split_offset_unchecked(index)
}

fn value<'i>(input: Span<'i>) -> TResult<'i, Option<MalVal>> {
    let bool = Map(bool, |v: bool| Some(MalVal::Bool(v)));
    let num = Map(num, Some);
    let string = Map(string, |s: &str| Some(MalVal::Str(unescape_str(s))));
    let atom = Or(Or(bool, num), string);

    let comment = Map(comment, |_: &str| None);
    let ident = Map(ident, Some);

    let list = Map(list, Some);
    let vec = Map(vec, Some);
    let map = Map(map, Some);
    let list_like = Or(Or(list, vec), map);

    let shorthand = Map(shorthand, Some);

    let value = Or(Or(Or(Or(list_like, comment), shorthand), atom), ident);
    value.parse(input)
}

fn shorthand<'i>(input: Span<'i>) -> TResult<'i, MalVal> {
    Or(
        Or(Shorthand("'", "quote"), Shorthand("`", "quasiquote")),
        Or(Shorthand("~@", "splice_unquote"), Shorthand("~", "unquote")),
    )
    .parse(input)
}

struct Shorthand(&'static str, &'static str);

impl<'i> Parser<'i> for Shorthand {
    type Output = MalVal;

    fn parse(self, input: Span<'i>) -> TResult<'i, Self::Output> {
        let (rest, _) = Tag(self.0).parse(input)?;
        let (rest, value) = next_value(rest)?;
        Ok((
            rest,
            MalVal::List(list!(MalVal::Sym(self.1.to_string()), value)),
        ))
    }
}

fn next_value<'i>(input: Span<'i>) -> TResult<'i, MalVal> {
    let (mut rest, mut val) = value(input)?;

    loop {
        match val {
            Some(val) => break Ok((rest, val)),
            None => (rest, val) = value(rest)?,
        }
    }
}

fn map<'i>(input: Span<'i>) -> TResult<'i, MalVal> {
    let (rest, ls) = ListLike("{", "}").parse(input)?;
    let mut ls = List::from_vec(ls);
    ls.push_front(MalVal::Sym("map".into()));
    Ok((rest, MalVal::List(ls)))
}

fn list<'i>(input: Span<'i>) -> TResult<'i, MalVal> {
    Map(ListLike("(", ")"), |ls: Vec<MalVal>| {
        MalVal::List(List::from_vec(ls))
    })
    .parse(input)
}

fn vec<'i>(input: Span<'i>) -> TResult<'i, MalVal> {
    Map(ListLike("[", "]"), MalVal::Vector).parse(input)
}

struct ListLike(&'static str, &'static str);

impl<'i> Parser<'i> for ListLike {
    type Output = Vec<MalVal>;

    fn parse(self, input: Span<'i>) -> TResult<'i, Self::Output> {
        let (rest, _) = Tag(self.0).parse(input)?;
        let (rest, value) = elems(rest)?;
        let (rest, _) = Tag(self.1).parse(rest)?;
        Ok((rest, value))
    }
}

fn elems<'i>(input: Span<'i>) -> TResult<'i, Vec<MalVal>> {
    let mut elems = vec![];
    let mut rest = input;

    while let Ok((r, elem)) = next_value(rest.clone()) {
        elems.push(elem);
        (rest, _) = r.take_while(|ch| ch.is_whitespace());
    }

    Ok((rest, elems))
}

fn ident<'i>(input: Span<'i>) -> TResult<'i, MalVal> {
    let keyword = Map(keyword, |s: &str| MalVal::Kwd(s.to_string()));
    let symbol = Map(symbol, |s: &str| MalVal::Sym(s.to_string()));
    Or(keyword, symbol).parse(input)
}

fn symbol<'i>(input: Span<'i>) -> TResult<'i, &'i str> {
    let Some((rest, symbol)) = input.take_while1(is_valid_char) else {
        return Err(input.err(ErrorKind::Symbol));
    };
    Ok((rest, symbol.data))
}

fn keyword<'i>(input: Span<'i>) -> TResult<'i, &'i str> {
    let (rest, _) = Tag(":").parse(input.clone())?;
    let Some((rest, keyword)) = rest.take_while1(is_valid_char) else {
        return Err(input.err(ErrorKind::Keyword));
    };
    Ok((rest, keyword.data))
}

fn is_valid_char(input: char) -> bool {
    !input.is_whitespace() && !['\"', '\'', '(', ')', '[', ']', '{', '}'].contains(&input)
}

fn string<'i>(input: Span<'i>) -> TResult<'i, &'i str> {
    let (rest, _) = Tag("\"").parse(input)?;

    let mut is_escaped = false;

    let (rest, string) = rest.take_while(|ch| {
        if is_escaped {
            is_escaped = false;
        } else if ch == '\\' {
            is_escaped = true;
        } else if ch == '\"' {
            return false;
        }
        true
    });

    let (rest, _) = Tag("\"").parse(rest)?;

    Ok((rest, string.data))
}

fn unescape_str(s: &str) -> String {
    s.replace(r#"\n"#, "\n")
        .replace(r#"\""#, "\"")
        .replace(r#"\\"#, "\\")
}

fn comment<'i>(input: Span<'i>) -> TResult<'i, &'i str> {
    let (rest, _) = Tag(";").parse(input)?;
    let (rest, comment) = rest.take_while(|ch| ch != '\n');
    Ok((rest, comment.data))
}

fn bool<'i>(input: Span<'i>) -> TResult<'i, bool> {
    Or(Map(Tag("true"), |_| true), Map(Tag("false"), |_| false)).parse(input)
}

fn num<'i>(input: Span<'i>) -> TResult<'i, MalVal> {
    let float = Map(float, MalVal::Float);
    let int = Map(int, MalVal::Int);
    Or(float, int).parse(input)
}

fn float<'i>(input: Span<'i>) -> TResult<'i, f64> {
    let (rest, int) = take_int(input.clone())?;
    let (rest, dot) = Tag(".").parse(rest)?;
    let digs = match digits(rest) {
        Ok((_, digs)) => digs.data.len(),
        Err(_) => 0,
    };

    let len = int.data.len() + dot.data.len() + digs;
    let (rest, float) = input.split_offset_unchecked(len);

    match float.data.replace('_', "").parse() {
        Ok(float) => Ok((rest, float)),
        Err(e) => Err(int.err(ErrorKind::Float(e))),
    }
}

fn int<'i>(input: Span<'i>) -> TResult<'i, i64> {
    let (rest, int) = take_int(input)?;

    match int.data.replace('_', "").parse() {
        Ok(int) => Ok((rest, int)),
        Err(e) => Err(int.err(ErrorKind::Int(e))),
    }
}

fn take_int<'i>(input: Span<'i>) -> SResult<'i> {
    let (rest, slen) = match Or(Tag("+"), Tag("-")).parse(input.clone()) {
        Ok((rest, sign)) => (rest, sign.data.len()),
        Err(_) => (input.clone(), 0),
    };

    let (_, digits) = digits(rest)?;

    let (rest, int) = input.split_offset_unchecked(slen + digits.data.len());

    Ok((rest, int))
}

fn digits<'i>(input: Span<'i>) -> SResult<'i> {
    let Some((rest, ch)) = input.take_one() else {
        return Err(input.err(ErrorKind::Digit));
    };

    if !ch.is_ascii_digit() {
        return Err(input.err(ErrorKind::Digit));
    }

    Ok(rest.take_while(|ch| ch.is_ascii_digit() || ch == '_'))
}

struct Map<P, F>(P, F);

impl<'i, P, F, T> Parser<'i> for Map<P, F>
where
    P: Parser<'i>,
    F: Fn(P::Output) -> T,
{
    type Output = T;

    fn parse(self, input: Span<'i>) -> TResult<'i, Self::Output> {
        let (rest, first) = self.0.parse(input)?;
        Ok((rest, self.1(first)))
    }
}

struct Tag(&'static str);

impl<'i> Parser<'i> for Tag {
    type Output = Span<'i>;

    fn parse(self, input: Span<'i>) -> SResult<'i> {
        let Some((rest, first)) = input.split_offset(self.0.len()) else {
            return Err(input.err(ErrorKind::Tag(self.0)));
        };

        if first.data == self.0 {
            Ok((rest, first))
        } else {
            Err(input.err(ErrorKind::Tag(self.0)))
        }
    }
}

struct Or<L, R>(L, R);

impl<'i, O, L, R> Parser<'i> for Or<L, R>
where
    L: Parser<'i, Output = O>,
    R: Parser<'i, Output = O>,
{
    type Output = O;

    fn parse(self, input: Span<'i>) -> TResult<'i, Self::Output> {
        let e0 = match self.0.parse(input.clone()) {
            Ok(res) => return Ok(res),
            Err(e) => e,
        };

        let e1 = match self.1.parse(input) {
            Ok(res) => return Ok(res),
            Err(e) => e,
        };

        Err(e0.or(e1))
    }
}

/// The input type for `Parser`, keeps track of the current line and column.
#[derive(Debug, Clone)]
struct Span<'i> {
    data: &'i str,
    line: usize,
    col: usize,
}

impl<'i> Span<'i> {
    fn split_offset(&self, offset: usize) -> Option<(Self, Self)> {
        if self.data.len() < offset {
            return None;
        }

        Some(self.split_offset_unchecked(offset))
    }

    fn split_offset_unchecked(&self, offset: usize) -> (Self, Self) {
        (
            Self {
                data: &self.data[offset..],
                line: self.data[..offset].chars().filter(|ch| *ch == '\n').count(),
                col: self.data[..offset].len() - self.data[..offset].rfind('\n').unwrap_or(0),
            },
            Self {
                data: &self.data[..offset],
                col: self.col,
                line: self.line,
            },
        )
    }

    fn take_one(&self) -> Option<(Self, char)> {
        let (i, ch) = self.data.char_indices().next()?;
        let (rest, _) = self.split_offset_unchecked(i);
        Some((rest, ch))
    }

    fn take_while1(&self, f: impl FnMut(char) -> bool) -> Option<(Self, Self)> {
        let (rest, first) = self.take_while(f);
        if first.data.len() == 0 {
            return None;
        }
        Some((rest, first))
    }

    fn take_while(&self, mut f: impl FnMut(char) -> bool) -> (Self, Self) {
        let chars = self.data.char_indices();
        let mut line = 0;
        let mut col = 0;

        for (offset, chr) in chars {
            if chr == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }

            if !f(chr) {
                return self.split_offset_unchecked(offset);
            }
        }

        (
            Self {
                data: "",
                line,
                col,
            },
            self.clone(),
        )
    }

    fn err(self, kind: ErrorKind) -> Error<'i> {
        Error {
            inner: ErrorInner::One(ErrorData { data: self, kind }),
        }
    }
}

type TResult<'i, T> = Result<(Span<'i>, T), Error<'i>>;
type SResult<'i> = Result<(Span<'i>, Span<'i>), Error<'i>>;

/// A type that takes a span and gives an output.
trait Parser<'i> {
    type Output;

    fn parse(self, input: Span<'i>) -> Result<(Span<'i>, Self::Output), Error<'i>>;
}

impl<'i, F: Fn(Span<'i>) -> TResult<'i, O>, O> Parser<'i> for F {
    type Output = O;

    #[inline]
    fn parse(self, input: Span<'i>) -> TResult<'i, O> {
        self(input)
    }
}

#[derive(thiserror::Error, Debug)]
#[error(transparent)]

pub struct Error<'i> {
    inner: ErrorInner<'i>,
}

impl<'i> Error<'i> {
    pub fn or(self, o: Self) -> Self {
        Error {
            inner: ErrorInner::Or(Box::new((self.inner, o.inner))),
        }
    }
}

#[derive(thiserror::Error, Debug)]
enum ErrorInner<'i> {
    #[error("{0}")]
    One(ErrorData<'i>),
    #[error("{} or {}", _0.0, _0.1)]
    Or(Box<(ErrorInner<'i>, ErrorInner<'i>)>),
}

#[derive(Display, Debug)]
#[display("{kind} at:{}:{}", data.line, data.col)]
struct ErrorData<'i> {
    data: Span<'i>,
    kind: ErrorKind,
}

#[derive(Debug, Display)]
enum ErrorKind {
    #[display("expected '{_0}'")]
    Tag(&'static str),
    #[display("digit")]
    Digit,
    #[display("keyword")]
    Keyword,
    #[display("symbol")]
    Symbol,
    Int(std::num::ParseIntError),
    Float(std::num::ParseFloatError),
    #[display("unexpected eof")]
    UnexpectedEof,
}
