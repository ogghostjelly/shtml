use derive_more::Display;
use nom::bytes::complete::escaped;
use nom::character::complete::{char, one_of};
use nom::combinator::value;
use nom::error::ParseError;

use nom::sequence::terminated;
use nom::{
    bytes::complete::{tag, take_till, take_while},
    character::is_newline,
    combinator::opt,
    sequence::preceded,
    Parser,
};
use nom::{InputTake, InputTakeAtPosition as _, Slice};
use nom_span::Spanned;

use crate::types::MalVal;

type Span<'i> = Spanned<&'i str>;

pub fn parse(input: &str) -> Result<Option<MalVal>, Error> {
    let span = Span::new(input, true);

    println!("inp: {}", span.data());
    println!("{:?}", string(span));

    Ok(None)
}

fn string(input: Span) -> IResult<Span, String> {
    fn str_chars(input: Span) -> IResult<Span, Span> {
        let (mut rest, mut chr) = input.take_split(1);

        loop {
            if *chr == "\\" {
                // Skip one
                _ = rest.take_split(1);
            } else if *chr == "\"" {
                // End
                break;
            }

            (rest, chr) = rest.take_split(1)
        }

        Ok(input.take_split(rest.byte_offset()))
    }

    fn parse_str(i: Span) -> IResult<Span, Span> {
        escaped(str_chars, '\\', one_of("\"n\\"))(i)
    }

    let (rest, string) = preceded(char('\"'), terminated(parse_str, char('\"')))(input)?;

    let string = string
        .replace(r#"\n"#, "\n")
        .replace(r#"\""#, "\"")
        .replace(r#"\\"#, "\\");

    Ok((rest, string))
}

fn bool(input: Span) -> IResult<Span, bool> {
    let t = value(true, tag("true"));
    let f = value(false, tag("false"));
    t.or(f).parse(input)
}

fn comment(input: Span) -> IResult<Span, Span> {
    preceded(char(';'), take_till(|c| is_newline(c as u8)))(input)
}

fn float(input: Span) -> IResult<Span, f64> {
    let (rest, _) = take_int(input)?;
    let (rest, _) = char('.')(rest)?;
    let (rest, _) = opt(take_int)(rest)?;
    let (rest, num) = input.take_split(rest.byte_offset());

    match num.replace('_', "").parse() {
        Ok(num) => Ok((rest, num)),
        Err(e) => Err(nom::Err::Error(Error::one(input, ErrorKind::ParseFloat(e)))),
    }
}

fn int(input: Span) -> IResult<Span, i64> {
    let (rest, num) = take_int(input)?;

    match num.replace('_', "").parse() {
        Ok(num) => Ok((rest, num)),
        Err(e) => Err(nom::Err::Error(Error::one(input, ErrorKind::ParseInt(e)))),
    }
}

fn take_int(input: Span) -> IResult<Span, Span> {
    let (rest, _) = opt(char('+').or(char('-'))).parse(input)?;
    let (rest, _) = take_while(|c| is_digit(c))(rest)?;
    Ok(input.take_split(rest.byte_offset()))
}

fn is_digit(chr: char) -> bool {
    nom::character::is_digit(chr as u8) || chr == '_'
}

pub type IResult<'i, I, O> = nom::IResult<I, O, Error<'i>>;

#[derive(Display, Debug)]
#[display("{}",
    _0.iter()
        .map(|(inp, e)| format!("error {e:?} at: {inp:?}"))
        .collect::<Vec<String>>()
        .join(" and ")
)]

pub struct Error<'i>(Vec<(Span<'i>, ErrorKind)>);

impl<'i> Error<'i> {
    pub fn one(input: Span<'i>, kind: ErrorKind) -> Self {
        Self(vec![(input, kind)])
    }
}

impl<'i> std::error::Error for Error<'i> {}

#[derive(Display, Debug)]
pub enum ErrorKind {
    ParseInt(std::num::ParseIntError),
    ParseFloat(std::num::ParseFloatError),
    #[display("{_0:?}")]
    Nom(nom::error::ErrorKind),
}

impl<'i> ParseError<Span<'i>> for Error<'i> {
    fn from_error_kind(input: Span<'i>, kind: nom::error::ErrorKind) -> Self {
        Self(vec![(input, ErrorKind::Nom(kind))])
    }

    fn append(input: Span<'i>, kind: nom::error::ErrorKind, mut other: Self) -> Self {
        other.0.push((input, ErrorKind::Nom(kind)));
        other
    }
}
