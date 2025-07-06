use derive_more::Display;

use crate::types::MalVal;

pub fn parse(input: &str) -> Result<Option<MalVal>, Error> {
    let input = Span { data: input };

    let res = bool(input).unwrap();

    println!("{res:?}");

    todo!()
}

fn bool<'i>(input: Span<'i>) -> TResult<'i, bool> {
    Or(
        Map(Tag("true"), |_: Span<'_>| true),
        Map(Tag("false"), |_: Span<'_>| false),
    )
    .parse(input)
}

fn take_int<'i>(input: Span<'i>) -> SResult<'i> {
    let rest = match Or(Tag("+"), Tag("-")).parse(input.clone()) {
        Ok((rest, _)) => rest,
        Err(_) => input,
    };

    let x = digits(rest)?;

    todo!()
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
            },
            Self {
                data: &self.data[..offset],
            },
        )
    }

    fn take_one(&self) -> Option<(Self, char)> {
        let (i, ch) = self.data.char_indices().next()?;
        let rest = Self {
            data: &self.data[i..],
        };
        Some((rest, ch))
    }

    fn take_while(&self, f: impl Fn(char) -> bool) -> (Self, Self) {
        let chars = self.data.char_indices();

        for (offset, chr) in chars {
            if !f(chr) {
                return self.split_offset_unchecked(offset);
            }
        }

        (Self::empty(), self.clone())
    }

    #[inline]
    fn empty() -> Self {
        Self { data: "" }
    }

    fn err(self, kind: ErrorKind) -> Error<'i> {
        Error::One(ErrorInner { data: self, kind })
    }
}

type TResult<'i, T> = Result<(Span<'i>, T), Error<'i>>;
type SResult<'i> = Result<(Span<'i>, Span<'i>), Error<'i>>;

/// A type that takes a span and gives an output.
trait Parser<'i> {
    type Output;

    fn parse(self, input: Span<'i>) -> Result<(Span<'i>, Self::Output), Error<'i>>;
}

impl<'i, T: Fn(Span) -> TResult<O>, O> Parser<'i> for T {
    type Output = O;

    #[inline]
    fn parse(self, input: Span<'i>) -> TResult<'i, O> {
        self(input)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum Error<'i> {
    #[error("{0}")]
    One(ErrorInner<'i>),
    #[error("{0:?}")]
    Or(Box<(Error<'i>, Error<'i>)>),
}

impl<'i> Error<'i> {
    pub fn or(self, o: Self) -> Self {
        Self::Or(Box::new((self, o)))
    }
}

#[derive(Display, Debug)]
#[display("error {kind:?} at: {data:?}")]
pub struct ErrorInner<'i> {
    data: Span<'i>,
    kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    Tag(&'static str),
    Digit,
    Int,
}
