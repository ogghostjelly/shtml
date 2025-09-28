use std::{io, rc::Rc};

use indexmap::IndexMap;

use crate::{
    list,
    reader::reader::Char,
    types::{Html, List, MalData, MalKey, MalVal},
};

use super::{
    reader::{Action, Reader},
    Location,
};

impl<I> Reader<I>
where
    I: Iterator<Item = Result<char>>,
{
    pub fn parse_file(&mut self) -> Result<Vec<Rc<MalData>>> {
        let mut ast = vec![];
        loop {
            self.skip_any_whitespace()?;
            if let Char::Eof = self.peek()? {
                return Ok(ast);
            }
            if let Some(value) = self.parse_value()? {
                ast.push(value);
            }
        }
    }

    fn parse_next_value(&mut self) -> Result<Rc<MalData>> {
        loop {
            if let Some(value) = self.parse_value()? {
                return Ok(value);
            }
        }
    }

    pub fn parse_value(&mut self) -> Result<Option<Rc<MalData>>> {
        self.flush();

        if self.take("~@")? {
            return self.parse_shorthand("splice-unquote");
        }
        match self.pop()? {
            Char::Char('\'') => return self.parse_shorthand("quote"),
            Char::Char('`') => return self.parse_shorthand("quasiquote"),
            Char::Char('~') => return self.parse_shorthand("unquote"),
            _ => {}
        }
        self.rewind();

        match self.parse_list()? {
            Some(value) => return Ok(Some(MalVal::List(value).with_loc(self.loc()))),
            None => self.rewind(),
        }

        match self.parse_vector()? {
            Some(value) => return Ok(Some(MalVal::Vector(value).with_loc(self.loc()))),
            None => self.rewind(),
        }

        match self.parse_map()? {
            Some(value) => return Ok(Some(MalVal::Map(value).with_loc(self.loc()))),
            None => self.rewind(),
        }

        match self.parse_string()? {
            Some(value) => return Ok(Some(MalVal::Str(value).with_loc(self.loc()))),
            None => self.rewind(),
        }

        match self.parse_comment()? {
            Some(()) => return Ok(None),
            None => self.rewind(),
        }

        match self.parse_html()? {
            Some(tag) => return Ok(Some(MalVal::Html(tag).with_loc(self.loc()))),
            None => self.rewind(),
        }

        self.parse_atom().map(Some)
    }

    fn parse_shorthand(&mut self, name: &'static str) -> Result<Option<Rc<MalData>>> {
        Ok(Some(
            list!(
                MalVal::Sym(name.to_string()).with_loc(self.loc()),
                self.parse_next_value()?
            )
            .with_loc(self.loc()),
        ))
    }

    fn parse_html(&mut self) -> Result<Option<Html>> {
        let Char::Char('<') = self.peek()? else {
            return Ok(None);
        };

        if self.take("<!DOCTYPE ")? {
            _ = self.take_while(|ch| ch != '>')?;
            let Char::Char('>') = self.pop()? else {
                return Err(Error::Expected('>'));
            };
            self.skip_any_whitespace()?;
        }

        let Some(tag) = self.parse_html_tag()? else {
            return Err(Error::InvalidHtml);
        };

        self.parse_html_inner(tag).map(Some)
    }

    fn parse_html_inner(&mut self, open: HtmlTag) -> Result<Html> {
        match open.tag_type {
            HtmlTagType::Open => {}
            HtmlTagType::Close | HtmlTagType::Void => {
                return Ok(Html {
                    tag: open.tag,
                    properties: open.properties,
                    children: None,
                });
            }
        }

        let start_loc = self.loc();
        let mut s = (String::new(), self.loc());
        let mut children = vec![];

        while let Char::Char(ch) = self.peek()? {
            self.flush();

            match self.parse_html_tag()? {
                Some(HtmlTag {
                    tag,
                    properties: _,
                    tag_type: HtmlTagType::Close,
                }) if tag == open.tag => {
                    return Ok(Html {
                        tag: open.tag,
                        properties: open.properties,
                        children: Some(children),
                    });
                }
                Some(tag) => {
                    children.push(MalVal::Str(std::mem::take(&mut s.0)).with_loc(s.1));
                    s.1 = self.loc();
                    children.push(MalVal::Html(self.parse_html_inner(tag)?).with_loc(self.loc()));
                }
                None => {
                    self.rewind();
                    s.0.push(ch);
                    self.next(ch);
                }
            }
        }

        Err(Error::HtmlUnclosedTag(open.tag, start_loc))
    }

    fn parse_html_tag(&mut self) -> Result<Option<HtmlTag>> {
        fn is_valid_tag_char(ch: char) -> bool {
            !ch.is_whitespace() && ch != '>' && ch != '/'
        }

        fn is_valid_prop_char(ch: char) -> bool {
            is_valid_tag_char(ch) && ch != '='
        }

        fn is_void_tag(tag: &str) -> bool {
            match tag {
                "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "keygen"
                | "link" | "meta" | "param" | "source" | "track" | "wbr" => true,
                _ => false,
            }
        }

        fn to_html_tag(
            tag: String,
            properties: Vec<(String, Option<String>)>,
            close: bool,
            void: bool,
        ) -> HtmlTag {
            if void {
                HtmlTag {
                    tag,
                    properties,
                    tag_type: HtmlTagType::Void,
                }
            } else if close {
                HtmlTag {
                    tag,
                    properties,
                    tag_type: HtmlTagType::Close,
                }
            } else {
                HtmlTag {
                    tag,
                    properties,
                    tag_type: HtmlTagType::Open,
                }
            }
        }

        // Take opening '<'
        let Char::Char('<') = self.pop()? else {
            return Ok(None);
        };

        // Check if it is a closing tag
        let close = self.pop_if(|ch| matches!(ch, Char::Char('/')))?;

        // Get the tag name.
        if !matches!(self.peek()?, Char::Char(ch) if ch.is_alphanumeric()) {
            return Ok(None);
        }
        let Some(tag) = self.take_while(is_valid_tag_char)? else {
            return Ok(None);
        };

        let void = is_void_tag(&tag);

        // Take tag properties.
        let mut properties = vec![];

        loop {
            self.skip_any_whitespace()?;

            if let Some(void) = self.parse_html_tag_end(close, void)? {
                return Ok(Some(to_html_tag(tag, properties, close, void)));
            }
            let key = self
                .take_while(is_valid_prop_char)?
                .ok_or(Error::Expected('>'))?;

            self.skip_any_whitespace()?;

            let value = if let Char::Char(tok @ '=') = self.peek()? {
                self.next(tok);
                Some(
                    self.take_while(is_valid_prop_char)?
                        .ok_or(Error::Expected('>'))?,
                )
            } else {
                None
            };

            properties.push((key, value));
        }
    }

    #[must_use]
    fn parse_html_tag_end(&mut self, close: bool, void: bool) -> Result<Option<bool>> {
        if !close && !void {
            if self.take("/>")? {
                return Ok(Some(true));
            };
        }
        if self.take(">")? {
            return Ok(Some(void));
        };

        Ok(None)
    }

    fn parse_list(&mut self) -> Result<Option<List>> {
        let mut ls = Vec::new();
        Ok(self
            .parse_list_like(('(', ')'), |value| Ok(ls.push(value)))?
            .map(|()| List::from_vec(ls)))
    }

    fn parse_vector(&mut self) -> Result<Option<Vec<Rc<MalData>>>> {
        let mut vec = Vec::new();
        Ok(self
            .parse_list_like(('[', ']'), |value| Ok(vec.push(value)))?
            .map(|()| vec))
    }

    fn parse_map(&mut self) -> Result<Option<IndexMap<MalKey, Rc<MalData>>>> {
        let mut map = IndexMap::new();
        let mut key: Option<MalKey> = None;

        let Some(()) = self.parse_list_like(('{', '}'), |value| {
            let Some(key) = key.take() else {
                return Ok(key = Some(match MalKey::from_value(value.value.clone()) {
                    Ok(key) => key,
                    Err(e) => return Err(Error::InvalidMapKey(e)),
                }));
            };

            match map.insert(key.clone(), value) {
                Some(value) => Err(Error::DuplicateMapKey(key, value.value.clone())),
                None => Ok(()),
            }
        })?
        else {
            return Ok(None);
        };

        match key {
            Some(key) => Err(Error::KeyWithoutValue(key)),
            None => Ok(Some(map)),
        }
    }

    #[must_use]
    fn parse_list_like(
        &mut self,
        (open, close): (char, char),
        mut f: impl FnMut(Rc<MalData>) -> Result<()>,
    ) -> Result<Option<()>> {
        match self.pop()? {
            Char::Char(ch) if ch == open => {}
            _ => return Ok(None),
        }

        loop {
            self.skip_any_whitespace()?;

            match self.peek()? {
                Char::Eof => return Err(Error::Expected(close)),
                Char::Char(ch) if ch == close => {
                    self.next(ch);
                    return Ok(Some(()));
                }
                Char::Char(_) => {}
            }

            f(self.parse_next_value()?)?;
        }
    }

    fn parse_string(&mut self) -> Result<Option<String>> {
        let Char::Char('"') = self.pop()? else {
            return Ok(None);
        };

        let mut s = String::new();

        loop {
            match self.pop()? {
                Char::Char('\\') => match self.pop()? {
                    Char::Char('\\') => s.push('\\'),
                    Char::Char('"') => s.push('\"'),
                    Char::Char('n') => s.push('\n'),
                    Char::Char('t') => s.push('\t'),
                    Char::Char('r') => s.push('\r'),
                    _ => return Err(Error::InvalidEscapeSequence),
                },
                Char::Char('"') => return Ok(Some(s)),
                Char::Char(ch) => s.push(ch),
                Char::Eof => return Err(Error::Expected('"')),
            }
        }
    }

    fn skip_any_whitespace(&mut self) -> Result<()> {
        self.scan(|ch| match ch {
            Char::Char(ch) if ch.is_whitespace() => Ok(Action::Next),
            _ => Ok(Action::ExHalt),
        })
    }

    fn parse_atom(&mut self) -> Result<Rc<MalData>> {
        let Some(symbol) = self.take_while(is_valid_char)? else {
            return Err(Error::InvalidChar);
        };

        if is_num(&symbol) {
            match symbol.parse() {
                Ok(x) => return Ok(MalVal::Int(x).with_loc(self.loc())),
                Err(e) => match e.kind() {
                    std::num::IntErrorKind::PosOverflow => {
                        return Err(Error::ParseNumberPosOverflow);
                    }
                    std::num::IntErrorKind::NegOverflow => {
                        return Err(Error::ParseNumberNegOverflow);
                    }
                    _ => {}
                },
            }

            match symbol.parse() {
                Ok(x) => return Ok(MalVal::Float(x).with_loc(self.loc())),
                Err(_) => {}
            }

            return Err(Error::ParseNumber);
        }

        if symbol.starts_with(':') {
            let mut kwd = symbol;
            kwd.remove(0);
            return Ok(MalVal::Kwd(kwd).with_loc(self.loc()));
        }

        match symbol.as_str() {
            "true" => return Ok(MalVal::Bool(true).with_loc(self.loc())),
            "false" => return Ok(MalVal::Bool(false).with_loc(self.loc())),
            "nil" => return Ok(MalVal::Nil.with_loc(self.loc())),
            _ => {}
        }

        Ok(MalVal::Sym(symbol).with_loc(self.loc()))
    }

    fn parse_comment(&mut self) -> Result<Option<()>> {
        let Char::Char(';') = self.pop()? else {
            return Ok(None);
        };

        self.scan(|ch| match ch {
            Char::Char('\n') | Char::Eof => Ok(Action::InHalt),
            Char::Char(_) => Ok(Action::Next),
        })?;

        self.skip_any_whitespace()?;

        Ok(Some(()))
    }
}

#[derive(Debug)]
struct HtmlTag {
    tag: String,
    properties: Vec<(String, Option<String>)>,
    tag_type: HtmlTagType,
}

#[derive(Debug)]
enum HtmlTagType {
    Open,
    Close,
    Void,
}

fn is_num(s: &str) -> bool {
    let mut chars = s.chars();

    match chars.next() {
        Some('+' | '-' | '.') => chars.next().is_some_and(|ch| ch.is_numeric()),
        Some(ch) if ch.is_numeric() => true,
        _ => false,
    }
}

pub fn is_valid_char(input: char) -> bool {
    !input.is_whitespace() && !"()[]{}<>\"\',".contains(input)
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("couldnt parse number")]
    ParseNumber,
    #[error("couldnt parse number: too big")]
    ParseNumberPosOverflow,
    #[error("couldnt parse number: too small")]
    ParseNumberNegOverflow,
    #[error("expected {0}")]
    Expected(char),
    #[error("invalid escape sequence")]
    InvalidEscapeSequence,
    #[error("'{}' cannot be a map key", _0.type_name())]
    InvalidMapKey(MalVal),
    #[error("duplicate map key")]
    DuplicateMapKey(MalKey, MalVal),
    #[error("map key without value")]
    KeyWithoutValue(MalKey),
    #[error("invalid char")]
    InvalidChar,
    #[error("invalid html")]
    InvalidHtml,
    #[error("unclosed html tag '{0}' at '{1}'")]
    HtmlUnclosedTag(String, Location),
}
