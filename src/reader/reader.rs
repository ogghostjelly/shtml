use std::io::{self, Bytes};

use unicode_reader::CodePoints;

use super::{
    parser::{Error, Result},
    Location,
};

/// A rewindable iterator.
pub struct Reader<I>
where
    I: Iterator<Item = Result<char>>,
{
    iter: I,
    buf: Vec<char>,
    buf_ptr: usize,
    loc: Location,
}

const BUF_CAPACITY: usize = 4;

impl<I> Reader<I>
where
    I: Iterator<Item = Result<char>>,
{
    pub fn new(iter: I, buf_capacity: usize, loc: Location) -> Self {
        Self {
            iter,
            buf: Vec::with_capacity(buf_capacity),
            buf_ptr: 0,
            loc,
        }
    }

    #[inline]
    pub fn from_iter(iter: I, loc: Location) -> Self {
        Self::new(iter, BUF_CAPACITY, loc)
    }
}

impl<R: io::Read> Reader<ResIter<CodePoints<Bytes<R>>>> {
    pub fn from_unicode_reader(reader: R, loc: Location) -> Self {
        Reader::from_iter(ResIter(CodePoints::from(reader)), loc)
    }
}

impl<U> Reader<OkIter<U>>
where
    U: Iterator<Item = char>,
{
    pub fn from_chars(chars: U, loc: Location) -> Self {
        Self::from_iter(OkIter(chars), loc)
    }
}

impl<I> Reader<I>
where
    I: Iterator<Item = Result<char>>,
{
    pub fn loc(&self) -> Location {
        self.loc.clone()
    }

    pub fn finalize(mut self) -> Location {
        self.flush();
        self.loc
    }

    /// Create a checkpoint at the current location.
    #[inline]
    pub fn flush(&mut self) {
        for ch in self.buf.drain(..self.buf_ptr) {
            if ch == '\n' {
                self.loc.col = 0;
                self.loc.line += 1;
            } else {
                self.loc.col += 1;
            }
        }

        self.rewind();
    }

    /// Rewind to the last checkpoint.
    #[inline]
    pub fn rewind(&mut self) {
        self.buf_ptr = 0;
    }

    #[must_use]
    pub fn take(&mut self, s: &str) -> Result<bool> {
        for (i, ch) in s.char_indices() {
            if Char::Char(ch) != self.pop()? {
                self.buf_ptr -= i + 1;
                assert!(self.buf_ptr <= self.buf.len());
                return Ok(false);
            }
        }

        Ok(true)
    }
}

impl<I> Reader<I>
where
    I: Iterator<Item = Result<char>>,
{
    pub fn pop_if(&mut self, f: impl FnOnce(Char) -> bool) -> Result<bool> {
        let ch = self.peek()?;
        let res = f(ch.clone());
        if res {
            if let Char::Char(ch) = ch {
                self.next(ch);
            }
        }
        Ok(res)
    }

    pub fn take_while(&mut self, mut f: impl FnMut(char) -> bool) -> Result<Option<String>> {
        let mut s = String::new();
        self.scan(|ch| match ch {
            Char::Char(ch) if f(ch) => {
                s.push(ch);
                Ok(Action::Next)
            }
            _ => Ok(Action::ExHalt),
        })?;
        if s.is_empty() {
            Ok(None)
        } else {
            Ok(Some(s))
        }
    }

    pub fn scan(&mut self, mut f: impl FnMut(Char) -> Result<Action>) -> Result<()> {
        loop {
            let value = self.peek()?;
            match f(value.clone())? {
                Action::Next => match value {
                    Char::Char(ch) => _ = self.next(ch),
                    Char::Eof => break,
                },
                Action::ExHalt => break,
                Action::InHalt => match value {
                    Char::Char(ch) => {
                        self.next(ch);
                        break;
                    }
                    Char::Eof => break,
                },
            }
        }
        Ok(())
    }

    pub fn peek(&mut self) -> Result<Char> {
        assert!(self.buf_ptr <= self.buf.len());

        if self.buf_ptr == self.buf.len() {
            let Some(value) = self.iter.next() else {
                return Ok(Char::Eof);
            };
            self.buf.push(value?);
            Ok(Char::Char(self.buf[self.buf.len() - 1]))
        } else {
            Ok(Char::Char(self.buf[self.buf_ptr]))
        }
    }

    pub fn next(&mut self, _: char) {
        self.buf_ptr += 1;
        assert!(self.buf_ptr <= self.buf.len());
    }

    pub fn pop(&mut self) -> Result<Char> {
        let value = self.peek()?;
        if let Char::Char(ch) = value {
            self.next(ch);
            Ok(value)
        } else {
            Ok(value)
        }
    }
}

pub struct OkIter<I: Iterator>(I);

impl<I: Iterator> Iterator for OkIter<I> {
    type Item = Result<I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(Ok(self.0.next()?))
    }
}

pub struct ResIter<I: Iterator>(I);

impl<I, T, E> Iterator for ResIter<I>
where
    I: Iterator<Item = std::result::Result<T, E>>,
    E: Into<Error>,
{
    type Item = Result<T>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.next()? {
            Ok(value) => Some(Ok(value)),
            Err(e) => Some(Err(e.into())),
        }
    }
}

#[derive(Debug)]
pub enum Action {
    Next,
    // Exclusive halt
    ExHalt,
    // Inclusive halt
    InHalt,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Char {
    Char(char),
    Eof,
}

#[cfg(test)]
mod test {
    use crate::reader::Location;

    use super::{Action, Char, Reader};

    #[test]
    fn pop_if_scan() {
        let mut iter = Reader::from_chars(
            ['a', 'b', 'c', 'd', 'e'].into_iter(),
            Location::file("repl"),
        );

        assert_eq!(iter.pop().unwrap(), Char::Char('a'));

        let mut out = String::new();

        iter.scan(|ch| match ch {
            Char::Char('b') => {
                out.push('A');
                Ok(Action::Next)
            }
            Char::Char('c') => {
                out.push('B');
                Ok(Action::Next)
            }
            Char::Char('d') => {
                out.push('C');
                Ok(Action::Next)
            }
            Char::Char('e') => Ok(Action::ExHalt),
            _ => panic!("unexpected value"),
        })
        .unwrap();

        assert_eq!(out, "ABC");
        assert_eq!(iter.pop().unwrap(), Char::Char('e'));
        assert_eq!(iter.pop().unwrap(), Char::Eof);
    }

    #[test]
    fn pop_and_rewind() {
        let mut iter = Reader::from_chars(
            ['a', 'b', 'c', 'd', 'e'].into_iter(),
            Location::file("repl"),
        );

        assert_eq!(iter.peek().unwrap(), Char::Char('a'));
        assert_eq!(iter.pop().unwrap(), Char::Char('a'));
        assert_eq!(iter.pop().unwrap(), Char::Char('b'));
        assert_eq!(iter.pop().unwrap(), Char::Char('c'));
        iter.rewind();
        assert_eq!(iter.pop().unwrap(), Char::Char('a'));
        assert_eq!(iter.pop().unwrap(), Char::Char('b'));
        assert_eq!(iter.pop().unwrap(), Char::Char('c'));

        iter.flush();
        assert_eq!(iter.pop().unwrap(), Char::Char('d'));
        assert_eq!(iter.pop().unwrap(), Char::Char('e'));
        iter.rewind();
        assert_eq!(iter.pop().unwrap(), Char::Char('d'));
        assert_eq!(iter.pop().unwrap(), Char::Char('e'));
    }
}
