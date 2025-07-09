use std::fmt;
use std::path::PathBuf;
use std::rc::Rc;

use derive_more::Display;

use crate::list;
use crate::types::{List, MalData, MalVal};

pub fn parse_file(loc: Location, input: &str) -> Result<Vec<Element>, Error> {
    shtml(Span { data: input, loc })
}

pub fn parse(loc: Location, input: &str) -> Result<Option<MalData>, Error> {
    let (rest, value) = value(Span { data: input, loc })?;

    if !rest.data.is_empty() {
        return Err(rest.err(ErrorKind::UnexpectedEof));
    }

    Ok(value)
}

fn shtml(input: Span<'_>) -> Result<Vec<Element>, Error<'_>> {
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
    #[display("@{}", _0.value)]
    Value(MalData),
}

#[derive(Display, Debug, Clone, PartialEq)]
#[display("{line}:{col} in {}", file.display())]
pub struct Location {
    file: Rc<PathBuf>,
    line: usize,
    col: usize,
}

impl Location {
    pub fn new(file: Rc<PathBuf>, line: usize, col: usize) -> Self {
        Self { file, line, col }
    }

    pub fn file(file: impl Into<PathBuf>) -> Self {
        Self {
            file: Rc::new(file.into()),
            line: 1,
            col: 1,
        }
    }
}

#[macro_export]
macro_rules! loc {
    () => {{
        use std::{path::PathBuf, rc::Rc};
        $crate::reader::Location::new(
            Rc::new(PathBuf::from(file!())),
            line!() as usize,
            column!() as usize,
        )
    }};
}

fn text(input: Span<'_>) -> (Span<'_>, Span<'_>) {
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

fn value(input: Span<'_>) -> TResult<'_, Option<MalData>> {
    let (input, _) = input.take_while(|ch| ch.is_whitespace());

    let bool = Map(bool, Some);
    let num = Map(num, Some);
    let string = Map(string, Some);
    let atom = or(or(bool, num), string);

    let comment = Map(comment, |_: &str| None);
    let ident = Map(ident, Some);

    let list = Map(list, Some);
    let vec = Map(vec, Some);
    let map = Map(hash_map, Some);
    let list_like = or(or(list, vec), map);

    let shorthand = Map(shorthand, Some);

    let value = or(or(or(or(list_like, comment), shorthand), atom), ident);
    value.parse(input)
}

fn shorthand(input: Span<'_>) -> TResult<'_, MalData> {
    Or(
        Or(Shorthand("'", "quote"), Shorthand("`", "quasiquote")),
        Or(Shorthand("~@", "splice-unquote"), Shorthand("~", "unquote")),
    )
    .parse(input)
}

struct Shorthand(&'static str, &'static str);

impl<'i> Parser<'i> for Shorthand {
    type Output = MalData;

    fn parse(self, input: Span<'i>) -> TResult<'i, Self::Output> {
        let loc = input.loc.clone();
        let (rest, tag) = Tag(self.0).parse(input)?;
        let (rest, value) = next_value(rest)?;
        Ok((
            rest,
            list!(MalVal::Sym(self.1.to_string()).with_loc(tag.loc), value).with_loc(loc),
        ))
    }
}

fn next_value(input: Span<'_>) -> TResult<'_, MalData> {
    let (mut rest, mut val) = value(input)?;

    loop {
        match val {
            Some(val) => break Ok((rest, val)),
            None => (rest, val) = value(rest)?,
        }
    }
}

fn hash_map(input: Span<'_>) -> TResult<'_, MalData> {
    let loc = input.loc.clone();
    let (rest, ls) = ListLike("{", "}").parse(input)?;
    let mut ls = List::from_vec(ls);
    ls.push_front(MalVal::Sym("hash-map".into()).with_loc(loc.clone()));
    Ok((rest, MalVal::List(ls).with_loc(loc)))
}

fn list(input: Span<'_>) -> TResult<'_, MalData> {
    let loc = input.loc.clone();
    Map(ListLike("(", ")"), |ls: Vec<MalData>| {
        MalVal::List(List::from_vec(ls)).with_loc(loc)
    })
    .parse(input)
}

fn vec(input: Span<'_>) -> TResult<'_, MalData> {
    let loc = input.loc.clone();
    Map(ListLike("[", "]"), |v| MalVal::Vector(v).with_loc(loc)).parse(input)
}

struct ListLike(&'static str, &'static str);

impl<'i> Parser<'i> for ListLike {
    type Output = Vec<MalData>;

    fn parse(self, input: Span<'i>) -> TResult<'i, Self::Output> {
        let (rest, _) = Tag(self.0).parse(input)?;
        let (rest, value) = elems(rest)?;
        let (rest, _) = Tag(self.1).parse(rest)?;
        Ok((rest, value))
    }
}

fn elems(input: Span<'_>) -> TResult<'_, Vec<MalData>> {
    let mut elems = vec![];
    let (mut rest, _) = input.take_while(|ch| ch.is_whitespace());

    while let Ok((r, elem)) = next_value(rest.clone()) {
        elems.push(elem);
        (rest, _) = r.take_while(|ch| ch.is_whitespace());
    }

    Ok((rest, elems))
}

fn ident(input: Span<'_>) -> TResult<'_, MalData> {
    let loc = input.loc.clone();
    let keyword = Map(keyword, |s: &str| MalVal::Kwd(s.to_string()));
    let symbol = Map(symbol, |s: &str| MalVal::Sym(s.to_string()));
    Map(Or(keyword, symbol), |val: MalVal| val.with_loc(loc)).parse(input)
}

fn symbol(input: Span<'_>) -> TResult<'_, &str> {
    let Some((rest, symbol)) = input.take_while1(is_valid_char) else {
        return Err(input.err(ErrorKind::Symbol));
    };
    Ok((rest, symbol.data))
}

fn keyword(input: Span<'_>) -> TResult<'_, &str> {
    let (rest, _) = Tag(":").parse(input.clone())?;
    let Some((rest, keyword)) = rest.take_while1(is_valid_char) else {
        return Err(input.err(ErrorKind::Keyword));
    };
    Ok((rest, keyword.data))
}

fn is_valid_char(input: char) -> bool {
    !input.is_whitespace() && !['\"', '\'', '(', ')', '[', ']', '{', '}'].contains(&input)
}

fn string(input: Span<'_>) -> TResult<'_, MalData> {
    let loc = input.loc.clone();
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

    Ok((rest, MalVal::Str(unescape_str(string.data)).with_loc(loc)))
}

fn unescape_str(s: &str) -> String {
    s.replace(r#"\n"#, "\n")
        .replace(r#"\""#, "\"")
        .replace(r#"\\"#, "\\")
}

fn comment(input: Span<'_>) -> TResult<'_, &str> {
    let (rest, _) = Tag(";").parse(input)?;
    let (rest, comment) = rest.take_while(|ch| ch != '\n');
    Ok((rest, comment.data))
}

fn bool(input: Span<'_>) -> TResult<'_, MalData> {
    let loc = input.loc.clone();
    let boo = Or(Map(Tag("true"), |_| true), Map(Tag("false"), |_| false));
    let val = Map(boo, |v| MalVal::Bool(v).with_loc(loc));
    val.parse(input)
}

fn num(input: Span<'_>) -> TResult<'_, MalData> {
    let loc = input.loc.clone();
    let float = Map(float, MalVal::Float);
    let int = Map(int, MalVal::Int);
    Map(Or(float, int), |val: MalVal| val.with_loc(loc)).parse(input)
}

fn float(input: Span<'_>) -> TResult<'_, f64> {
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

fn int(input: Span<'_>) -> TResult<'_, i64> {
    let (rest, int) = take_int(input)?;

    match int.data.replace('_', "").parse() {
        Ok(int) => Ok((rest, int)),
        Err(e) => Err(int.err(ErrorKind::Int(e))),
    }
}

fn take_int(input: Span<'_>) -> SResult<'_> {
    let (rest, slen) = match Or(Tag("+"), Tag("-")).parse(input.clone()) {
        Ok((rest, sign)) => (rest, sign.data.len()),
        Err(_) => (input.clone(), 0),
    };

    let (_, digits) = digits(rest)?;

    let (rest, int) = input.split_offset_unchecked(slen + digits.data.len());

    Ok((rest, int))
}

fn digits(input: Span<'_>) -> SResult<'_> {
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
    F: FnOnce(P::Output) -> T,
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

fn or<'i, O, L, R>(l: L, r: R) -> Or<L, R>
where
    L: Parser<'i, Output = O>,
    R: Parser<'i, Output = O>,
{
    Or(l, r)
}

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
#[derive(Clone, PartialEq)]
struct Span<'i> {
    data: &'i str,
    loc: Location,
}

impl fmt::Debug for Span<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span(")?;
        write!(f, "{:?} at {}", self.data, self.loc)?;
        write!(f, ")")
    }
}

impl<'i> Span<'i> {
    fn split_offset(&self, offset: usize) -> Option<(Self, Self)> {
        if self.data.len() < offset {
            return None;
        }

        Some(self.split_offset_unchecked(offset))
    }

    fn split_offset_unchecked(&self, offset: usize) -> (Self, Self) {
        let col = match self.data[..offset].rfind('\n') {
            Some(n) => self.data[..offset].len() - n,
            None => self.data[..offset].len() + self.loc.col,
        };

        (
            Self {
                data: &self.data[offset..],
                loc: Location {
                    file: Rc::clone(&self.loc.file),
                    line: self.loc.line
                        + self.data[..offset].chars().filter(|ch| *ch == '\n').count(),
                    col,
                },
            },
            Self {
                data: &self.data[..offset],
                loc: Location {
                    file: Rc::clone(&self.loc.file),
                    col: self.loc.col,
                    line: self.loc.line,
                },
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
        if first.data.is_empty() {
            return None;
        }
        Some((rest, first))
    }

    fn take_while(&self, mut f: impl FnMut(char) -> bool) -> (Self, Self) {
        let chars = self.data.char_indices();

        for (offset, chr) in chars {
            if !f(chr) {
                return self.split_offset_unchecked(offset);
            }
        }

        self.split_offset_unchecked(self.data.len())
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

impl Error<'_> {
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
#[display("{kind} at {}", data.loc)]
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

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use super::{Location, Span};

    #[test]
    fn split_offset_unchecked() {
        let line = 1;
        let col = 1;

        let input = Span {
            data: "ABC\nDEF\nGHI",
            loc: Location {
                file: Rc::new("test".into()),
                line,
                col,
            },
        };

        assert_eq!(
            input.split_offset_unchecked(2),
            (
                Span {
                    data: "C\nDEF\nGHI",
                    loc: Location {
                        file: Rc::new("test".into()),
                        line,
                        col: col + 2
                    }
                },
                Span {
                    data: "AB",
                    loc: Location {
                        file: Rc::new("test".into()),
                        line,
                        col,
                    }
                }
            )
        );

        assert_eq!(
            input.split_offset_unchecked(5),
            (
                Span {
                    data: "EF\nGHI",
                    loc: Location {
                        file: Rc::new("test".into()),
                        line: line + 1,
                        col: 2
                    }
                },
                Span {
                    data: "ABC\nD",
                    loc: Location {
                        file: Rc::new("test".into()),
                        line,
                        col,
                    }
                },
            )
        );

        assert_eq!(
            input.split_offset_unchecked(8),
            (
                Span {
                    data: "GHI",
                    loc: Location {
                        file: Rc::new("test".into()),
                        line: line + 2,
                        col: 1,
                    }
                },
                Span {
                    data: "ABC\nDEF\n",
                    loc: Location {
                        file: Rc::new("test".into()),
                        line,
                        col,
                    }
                },
            )
        )
    }

    #[test]
    fn take_while() {
        let input = Span {
            data: "ABC\nDEF\nGHI",
            loc: Location::file("test"),
        };

        assert_eq!(
            input.take_while(|ch| ch != 'G'),
            (
                Span {
                    data: "GHI",
                    loc: Location {
                        file: Rc::new("test".into()),
                        line: 3,
                        col: 1
                    }
                },
                Span {
                    data: "ABC\nDEF\n",
                    loc: Location {
                        file: Rc::new("test".into()),
                        line: 1,
                        col: 1
                    }
                }
            )
        );

        assert_eq!(
            input.take_while(|_| true),
            (
                Span {
                    data: "",
                    loc: Location {
                        file: Rc::new("test".into()),
                        line: 3,
                        col: 4
                    }
                },
                Span {
                    data: "ABC\nDEF\nGHI",
                    loc: Location {
                        file: Rc::new("test".into()),
                        line: 1,
                        col: 1
                    }
                }
            )
        );
    }
}
