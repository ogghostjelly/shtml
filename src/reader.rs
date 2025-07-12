use std::fmt;
use std::rc::Rc;

use derive_more::Display;
use indexmap::IndexMap;

use crate::list;
use crate::types::{List, MalData, MalKey, MalVal};

pub fn parse_file(loc: Location, input: &str) -> Result<Vec<Element>, Error> {
    shtml(Span { data: input, loc })
}

pub fn parse(loc: Location, input: &str) -> Result<Vec<Rc<MalData>>, Error> {
    if input.is_empty() {
        return Ok(vec![]);
    }

    let mut list = Vec::new();
    let mut input = Span { data: input, loc };

    loop {
        let (rest, value) = value(input)?;
        if let Some(value) = value {
            list.push(value);
        }

        let (rest, _) = rest.take_while(|ch| ch.is_whitespace());
        if rest.data.is_empty() {
            break;
        }

        input = rest;
    }

    Ok(list)
}

fn shtml(input: Span<'_>) -> Result<Vec<Element>, Error<'_>> {
    let (mut input, mut elems) = (input, vec![]);

    loop {
        let (rest, text) = text(input);
        elems.push(Element::Text(unescape_text(text.data)));

        if rest.data.is_empty() {
            return Ok(elems);
        }

        let (rest, is_value) =
            Or(Map(Tag("@"), |_| true), Map(Tag("<x@"), |_| false)).parse(rest)?;

        let (rest, value) = if is_value {
            value(rest)
        } else {
            Map(shtml_tag, Some).parse(rest)
        }?;

        input = rest;

        if let Some(value) = value {
            elems.push(Element::Value(value));
        }
    }
}

fn shtml_tag(input: Span<'_>) -> TResult<'_, Rc<MalData>> {
    let (rest, _) = input.take_while(|ch| ch.is_whitespace());

    let sym_loc = rest.loc.clone();
    let (mut next, sym) = symbol(rest)?;
    let sym = MalVal::Sym(sym.to_string()).with_loc(sym_loc);

    let mut map = IndexMap::new();

    loop {
        let (rest, _) = next.take_while(|ch| ch.is_whitespace());

        let close = Map(Or(Tag(">"), Tag("/>")), |_| None);
        let (rest, pair) = Or(close, Map(shtml_tag_prop, Some)).parse(rest)?;

        next = rest;

        match pair {
            Some((key, value)) => _ = map.insert(key, value),
            None => break,
        }
    }

    let map = MalVal::Map(map).with_loc(input.loc.clone());
    Ok((next, list!(sym, map).with_loc(input.loc)))
}

fn shtml_tag_prop(input: Span<'_>) -> TResult<'_, (MalKey, Rc<MalData>)> {
    let (rest, key) = input.take_while(|ch| is_valid_char(ch) && ch != '=');
    let key = MalKey::Sym(key.data.to_string());

    let (rest, _) = rest.take_while(|ch| ch.is_whitespace());
    let (rest, _) = Tag("=").parse(rest)?;
    let (rest, _) = rest.take_while(|ch| ch.is_whitespace());

    let (rest, value) = value(rest)?;
    let value = value.expect("That is not a valid key!"); // TODO: Don't unwrap

    Ok((rest, (key, value)))
}

#[derive(Debug, Display)]
pub enum Element {
    Text(String),
    #[display("@{}", _0.value)]
    Value(Rc<MalData>),
}

#[derive(Display, Debug, Clone, PartialEq)]
#[display("{line}:{col} in {file}")]
pub struct Location {
    file: Rc<String>,
    line: usize,
    col: usize,
}

impl Location {
    pub fn new(file: Rc<String>, line: usize, col: usize) -> Self {
        Self { file, line, col }
    }

    pub fn file(file: impl Into<String>) -> Self {
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
        use std::rc::Rc;
        $crate::reader::Location::new(
            Rc::new(file!().into()),
            line!() as usize,
            column!() as usize,
        )
    }};
}

fn text(input: Span<'_>) -> (Span<'_>, Span<'_>) {
    let mut chars = input.data.char_indices().peekable();

    while let Some((i, ch)) = chars.next() {
        if input.data[i..].starts_with("<x@") {
            return input.split_offset_unchecked(i);
        } else if ch == '@' {
            if let Some((_, '@')) = chars.peek() {
                _ = chars.next();
            } else {
                return input.split_offset_unchecked(i);
            }
        }
    }

    input.split_offset_unchecked(input.data.len())
}

fn unescape_text(text: &str) -> String {
    text.replace("@@", "@")
}

fn value(input: Span<'_>) -> TResult<'_, Option<Rc<MalData>>> {
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

fn shorthand(input: Span<'_>) -> TResult<'_, Rc<MalData>> {
    Or(
        Or(Shorthand("'", "quote"), Shorthand("`", "quasiquote")),
        Or(Shorthand("~@", "splice-unquote"), Shorthand("~", "unquote")),
    )
    .parse(input)
}

struct Shorthand(&'static str, &'static str);

impl<'i> Parser<'i> for Shorthand {
    type Output = Rc<MalData>;

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

fn next_value(input: Span<'_>) -> TResult<'_, Rc<MalData>> {
    let (mut rest, mut val) = value(input)?;

    loop {
        match val {
            Some(val) => break Ok((rest, val)),
            None => (rest, val) = value(rest)?,
        }
    }
}

fn hash_map(input: Span<'_>) -> TResult<'_, Rc<MalData>> {
    let loc = input.loc.clone();
    let (rest, _) = Tag("{").parse(input)?;

    let (mut rest, _) = rest.take_while(|ch| ch.is_whitespace());
    let mut map = IndexMap::new();
    let mut key = None;

    while let Ok((r, elem)) = next_value(rest.clone()) {
        match key.take() {
            Some((_, key)) => _ = map.insert(key, elem),
            None => key = Some(to_key(rest, elem)?),
        }

        (rest, _) = r.take_while(|ch| ch.is_whitespace());
    }

    if let Some((inp, _)) = key {
        return Err(inp.err(ErrorKind::MapUnevenArgs));
    }

    let (rest, _) = Tag("}").parse(rest)?;
    Ok((rest, MalVal::Map(map).with_loc(loc)))
}

fn to_key(input: Span<'_>, data: Rc<MalData>) -> TResult<'_, MalKey> {
    match MalKey::from_value(data.value.clone()) {
        Ok(key) => Ok((input, key)),
        Err(value) => Err(input.err(ErrorKind::MapBadKey(value.type_name()))),
    }
}

fn list(input: Span<'_>) -> TResult<'_, Rc<MalData>> {
    let loc = input.loc.clone();
    Map(ListLike("(", ")"), |ls: Vec<Rc<MalData>>| {
        MalVal::List(List::from_vec(ls)).with_loc(loc)
    })
    .parse(input)
}

fn vec(input: Span<'_>) -> TResult<'_, Rc<MalData>> {
    let loc = input.loc.clone();
    Map(ListLike("[", "]"), |v| MalVal::Vector(v).with_loc(loc)).parse(input)
}

struct ListLike(&'static str, &'static str);

impl<'i> Parser<'i> for ListLike {
    type Output = Vec<Rc<MalData>>;

    fn parse(self, input: Span<'i>) -> TResult<'i, Self::Output> {
        let (rest, _) = Tag(self.0).parse(input)?;
        let (rest, value) = elems(rest)?;
        let (rest, _) = Tag(self.1).parse(rest)?;
        Ok((rest, value))
    }
}

fn elems(input: Span<'_>) -> TResult<'_, Vec<Rc<MalData>>> {
    let mut elems = vec![];
    let (mut rest, _) = input.take_while(|ch| ch.is_whitespace());

    while let Ok((r, elem)) = next_value(rest.clone()) {
        elems.push(elem);
        (rest, _) = r.take_while(|ch| ch.is_whitespace());
    }

    Ok((rest, elems))
}

fn ident(input: Span<'_>) -> TResult<'_, Rc<MalData>> {
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
    !input.is_whitespace() && !"()[]{}<>\"\'".contains(input)
}

fn string(input: Span<'_>) -> TResult<'_, Rc<MalData>> {
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

fn bool(input: Span<'_>) -> TResult<'_, Rc<MalData>> {
    let loc = input.loc.clone();
    let boo = Or(Map(Tag("true"), |_| true), Map(Tag("false"), |_| false));
    let val = Map(boo, |v| MalVal::Bool(v).with_loc(loc));
    val.parse(input)
}

fn num(input: Span<'_>) -> TResult<'_, Rc<MalData>> {
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
            inner: ErrorInner { data: self, kind },
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
        if self.inner.kind.priority() >= o.inner.kind.priority() {
            self
        } else {
            o
        }
    }
}

#[derive(thiserror::Error, Debug)]
#[error("{kind} at {}", data.loc)]
struct ErrorInner<'i> {
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
    #[display("'{_0}' cannot be a map key")]
    MapBadKey(&'static str),
    #[display("hash-map expects an even number of arguments")]
    MapUnevenArgs,
}

impl ErrorKind {
    fn priority(&self) -> usize {
        match self {
            ErrorKind::Tag(_) => 0,
            ErrorKind::Digit => 1,
            ErrorKind::Keyword => 1,
            ErrorKind::Symbol => 1,
            ErrorKind::Int(_) => 1,
            ErrorKind::Float(_) => 1,
            ErrorKind::MapBadKey(_) => 1,
            ErrorKind::MapUnevenArgs => 1,
        }
    }
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
