//! Parsing our format strings.
//!
//! Format strings are arbitrary strings containing 'field markers', indicating
//! the location of fields. Field markers begin with `#{` and end with `}`,
//! and contain the name of a struct field, or the index of a tuple field.
//!
//! Field markers must be separated by one or more dividing characters.
//! During parsing, we scan the input looking for these dividing strings,
//! which are discarded; the remaining subsections are associated with the
//! matching fields, and parsed based on the field type.
//!
//! # Examples
//!
//!```ignore
//! // A rect, represented as a space-delimited x, y, width, height
//! // example input: "10 11 320 104"
//!
//! #[pars::fmt("#{x} #{y} #{width} #{height}")]
//! struct Rect {
//!     x: usize,
//!     y: usize,
//!     width: usize,
//!     height: usize,
//! }
//!
//!```

use std::ops::Range;

pub type Error = &'static str;

enum State {
    Ready,
    InField,
    /// member is the start position
    InSeparator(usize),
    Finished,
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Field(String),
    Separator(String),
}

struct Parser<'a> {
    source: &'a str,
    pos: usize,
    state: State,
    tokens: Vec<Token>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Parser { source, pos: 0, state: State::Ready, tokens: Vec::new() }
    }

    fn run(&mut self) -> Result<(), Error> {
        loop {
            match self.state {
                State::Ready => self.advance()?,
                State::InField => self.take_field()?,
                State::InSeparator(start_pos) => self.take_separator(start_pos)?,
                State::Finished => break,
            }
        }
        Ok(())
    }

    fn advance(&mut self) -> Result<(), Error> {
        let (pos, next_state) = match self.source.as_bytes().get(self.pos) {
            Some(&b'#') if self.source.as_bytes().get(self.pos + 1) == Some(&b'{') => {
                if self.source.as_bytes().get(self.pos + 2).is_none() {
                    return Err("expected identifer, found EOF");
                }

                if let Some(Token::Field(_)) = self.tokens.last() {
                    return Err("fields must be separated by non-field characters");
                }

                (self.pos + 2, State::InField)
            }
            Some(b'#') => (self.pos + 1, State::InSeparator(self.pos)),
            Some(_other) => (self.pos, State::InSeparator(self.pos)),
            None => (self.pos, State::Finished),
        };

        self.pos = pos;
        self.state = next_state;
        Ok(())
    }

    fn take_field(&mut self) -> Result<(), Error> {
        let field_end = self.source.as_bytes()[self.pos..].iter().position(|b| b == &b'}');
        match field_end {
            Some(end) => {
                let token = Token::Field(self.source[self.pos..self.pos + end].to_string());
                self.tokens.push(token);
                self.pos = self.pos + end + 1;
                self.state = State::Ready;
                Ok(())
            }
            None => Err("expected '}' found end of string"),
        }
    }

    fn take_separator(&mut self, start_pos: usize) -> Result<(), Error> {
        let end_pos = start_pos
            + self.source.as_bytes()[self.pos..]
                .iter()
                .position(|b| b == &b'#')
                .unwrap_or(self.source.len() - self.pos);

        let sep_string = self.source[start_pos..end_pos].to_string();
        assert!(!sep_string.is_empty());
        if let Some(Token::Separator(ref mut existing)) = self.tokens.last_mut() {
            existing.push_str(&sep_string);
        } else {
            self.tokens.push(Token::Separator(sep_string));
        }
        self.pos = end_pos;
        self.state = State::Ready;
        Ok(())
    }

    fn into_matcher(self) -> Result<FmtMatcher, Error> {
        let tokens = self.tokens;
        let lead_separator = if let Some(Token::Separator(s)) = tokens.first() {
            Some(s.clone())
        } else {
            None
        };

        let mut fmt_fields = Vec::new();
        let skip = if lead_separator.is_some() { 1 } else { 0 };
        let mut iter = tokens.into_iter().skip(skip);
        loop {
            match (iter.next(), iter.next()) {
                (Some(Token::Field(field)), Some(Token::Separator(sep))) => fmt_fields.push((field, sep)),
                (Some(Token::Field(field)), None) => fmt_fields.push((field, String::new())),
                (None, None) => break,
                _other => return Err("unexpected token"),
            }
        }

        Ok(FmtMatcher {
            lead_separator,
            fmt_fields,
            fields: Vec::new(),
        })
    }
}

#[allow(dead_code)]
pub struct FmtMatcher {
    lead_separator: Option<String>,
    fmt_fields: Vec<(String, String)>,
    fields: Vec<String>,
}

#[allow(dead_code)]
pub struct FmtMatch<'a> {
    matcher: &'a FmtMatcher,
    source: &'a str,
    values: Vec<Range<usize>>,
}

impl FmtMatcher {
    pub fn new<S: AsRef<str>>(fmt_string: &str, _fields: &[S]) -> Result<Self, Error> {
        let mut parser = Parser::new(fmt_string);
        parser.run()?;
        parser.into_matcher()
    }

    pub fn try_match<'a>(&'a self, source: &'a str) -> Result<FmtMatch<'a>, Error> {
        let mut values = Vec::new();
        let mut pos = 0;

        if let Some(ref head) = self.lead_separator {
            match source.find(head) {
                Some(0) => pos = head.len(),
                _ => return Err("failed to find leading separator"),
            }
        }

        for (_field, sep) in self.fmt_fields.iter() {

            if pos == source.len() {
                return Err("input string exhausted with fields remaining");
            }
            if sep.is_empty() {
                // take all remaining string
                values.push(pos..source.len());
                pos = source.len();
                continue;
            }

            match &source[pos..].find(sep.as_str()) {
                Some(idx) => {
                    values.push(pos..pos + idx);
                    pos = pos + idx + sep.len();
                }
                None => return Err("failed to find separator"),
            }
        }

        Ok(FmtMatch {
            matcher: self,
            source,
            values,
        })
    }
}

impl<'a> FmtMatch<'a> {
    pub fn get_match(&'a self, idx: usize) -> Result<&'a str, Error> {
        let range = self.values.get(idx).ok_or("no match for index")?;
        Ok(&self.source[range.clone()])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! fld {
        ($s:expr) => {
            Token::Field($s.into())
        };
    }

    macro_rules! sep {
        ($s:expr) => {
            Token::Separator($s.into())
        };
    }

    #[test]
    fn test_simple_parse() {
        let mut parser = Parser::new("#{x}, #{y} #{width} #{height}");
        assert_eq!(parser.run(), Ok(()));
        assert_eq!(
            parser.tokens,
            vec![
                fld!("x"),
                sep!(", "),
                fld!("y"),
                sep!(" "),
                fld!("width"),
                sep!(" "),
                fld!("height")
            ]
        );
    }

    #[test]
    fn test_fail_with_unseparated_fields() {
        let mut parser = Parser::new("#{x}#{y} #{width} #{height}");
        assert!(parser.run().is_err());
    }

    #[test]
    fn make_matcher() {
        let mut parser = Parser::new("#{x}, #{y} #{width} #{height}");
        parser.run().unwrap();
        let matcher = parser.into_matcher().unwrap();
        assert!(matcher.lead_separator.is_none());
        assert_eq!(matcher.fmt_fields.len(), 4);

        let amatch = matcher.try_match("4, 5 10 11").unwrap();
        assert_eq!(amatch.values, vec![0..1, 3..4, 5..7, 8..10])
    }

    #[test]
    fn match_should_fail() {
        let mut parser = Parser::new("#{x}, #{y} #{width} #{height}");
        parser.run().unwrap();
        let matcher = parser.into_matcher().unwrap();
        assert!(matcher.try_match("4 5 hello").is_err());
    }
}
