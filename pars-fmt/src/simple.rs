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

use crate::error::{FormatError, MatchError};

#[derive(Debug)]
enum State {
    Ready,
    InField,
    /// member is the start position
    InSeparator(usize),
    Finished,
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Field(Range<usize>),
    Separator(Range<usize>),
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

    fn run(&mut self) -> Result<(), FormatError> {
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

    fn advance(&mut self) -> Result<(), FormatError> {
        let (pos, next_state) = match self.source.as_bytes().get(self.pos) {
            Some(&b'#') if self.source.as_bytes().get(self.pos + 1) == Some(&b'{') => {
                if self.source.as_bytes().get(self.pos + 2).is_none() {
                    return Err(FormatError::new(
                        self.source,
                        self.source.len()..self.source.len(),
                        "expected identifier, found end of string",
                    ));
                }

                if let Some(Token::Field(prev)) = self.tokens.last() {
                    return Err(FormatError::new(
                        self.source,
                        prev.start..self.pos + 2,
                        "fields must be separated by non-field characters",
                    ));
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

    fn take_field(&mut self) -> Result<(), FormatError> {
        let field_end = self.source.as_bytes()[self.pos..].iter().position(|b| b == &b'}');
        match field_end {
            Some(end) => {
                let token = Token::Field(self.pos..self.pos + end);
                self.tokens.push(token);
                self.pos = self.pos + end + 1;
                self.state = State::Ready;
                Ok(())
            }
            None => Err(FormatError::new(
                self.source,
                self.pos..self.source.len(),
                "expected '}' found end of string",
            )),
        }
    }

    fn take_separator(&mut self, start_pos: usize) -> Result<(), FormatError> {
        let end_pos = self.pos
            + self.source.as_bytes()[self.pos..]
                .iter()
                .position(|b| b == &b'#')
                .unwrap_or(self.source.len() - self.pos);

        let sep_range = start_pos..end_pos;
        assert!(start_pos < end_pos);
        if let Some(Token::Separator(ref mut existing)) = self.tokens.last_mut() {
            existing.end = end_pos;
        } else {
            self.tokens.push(Token::Separator(sep_range));
        }
        self.pos = end_pos;
        self.state = State::Ready;
        Ok(())
    }

    fn into_matcher(self) -> Result<FmtMatcher<'a>, FormatError> {
        let tokens = self.tokens;
        let lead_separator =
            if let Some(Token::Separator(s)) = tokens.first() { Some(s.clone()) } else { None };

        let mut fmt_fields = Vec::new();
        let skip = if lead_separator.is_some() { 1 } else { 0 };
        let mut iter = tokens.into_iter().skip(skip);
        loop {
            match (iter.next(), iter.next()) {
                (Some(Token::Field(field)), Some(Token::Separator(sep))) => {
                    fmt_fields.push((field, sep))
                }
                (Some(Token::Field(field)), None) => {
                    fmt_fields.push((field, self.source.len()..self.source.len()))
                }
                (Some(Token::Field(rng)), _) | (Some(Token::Separator(rng)), _) => {
                    return Err(FormatError::new(self.source, rng, "unexpected token"));
                }
                (None, None) => break,
                (None, Some(_)) => unreachable!(),
            }
        }

        Ok(FmtMatcher { source: self.source, lead_separator, fmt_fields, fields: Vec::new() })
    }

    #[cfg(test)]
    fn resolve_tokens(&'a self) -> Vec<&'a str> {
        self.tokens
            .iter()
            .map(|t| match t {
                Token::Field(rng) => &self.source[rng.clone()],
                Token::Separator(rng) => &self.source[rng.clone()],
            })
            .collect()
    }
}

#[allow(dead_code)]
pub struct FmtMatcher<'a> {
    source: &'a str,
    lead_separator: Option<Range<usize>>,
    //NOTE: we represent all substrings as ranges of self.source, to avoid
    //any unnecessary allocation.
    fmt_fields: Vec<(Range<usize>, Range<usize>)>,
    fields: Vec<String>,
}

#[allow(dead_code)]
pub struct FmtMatch<'a, 'b> {
    matcher: &'a FmtMatcher<'a>,
    source: &'b str,
    values: Vec<Range<usize>>,
}

impl<'a> FmtMatcher<'a> {
    pub fn new<S: AsRef<str>>(fmt_string: &'a str, _fields: &[S]) -> Result<Self, FormatError> {
        let mut parser = Parser::new(fmt_string);
        parser.run()?;
        parser.into_matcher()
    }

    pub fn try_match<'b>(&'a self, source: &'b str) -> Result<FmtMatch<'a, 'b>, MatchError> {
        let mut values = Vec::new();
        let mut pos = 0;
        let mut current_sep = 0;

        if let Some(ref head) = self.lead_separator {
            let sep_string = &self.source[head.clone()];
            match source.find(sep_string) {
                Some(0) => pos = head.len(),
                _ => return Err(MatchError::missing_separator(current_sep, sep_string)),
            }
            current_sep += 1;
        }

        for (_field, sep) in self.fmt_fields.iter() {
            let sep_string = &self.source[sep.clone()];

            if pos == source.len() {
                return Err(MatchError::InputExhausted);
            }
            if sep.start == sep.end {
                // take all remaining string
                values.push(pos..source.len());
                pos = source.len();
                continue;
            }

            match &source[pos..].find(sep_string) {
                Some(idx) => {
                    values.push(pos..pos + idx);
                    pos = pos + idx + sep.len();
                }
                None => return Err(MatchError::missing_separator(current_sep, sep_string)),
            }

            current_sep += 1;
        }

        Ok(FmtMatch { matcher: self, source, values })
    }
}

impl<'a, 'b> FmtMatch<'a, 'b> {
    pub fn get_match(&'a self, idx: usize) -> Result<&'a str, MatchError> {
        let range = self.values.get(idx).expect("all indices should be validated by now");
        Ok(&self.source[range.clone()])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_parse() {
        let mut parser = Parser::new("#{x}, #{y} #{width} #{height}");
        assert_eq!(parser.run(), Ok(()));
        assert_eq!(parser.resolve_tokens(), vec!["x", ", ", "y", " ", "width", " ", "height"]);
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
    #[should_panic(expected = "MissingSeparator")]
    fn match_should_fail() {
        let mut parser = Parser::new("#{x}, #{y} #{width} #{height}");
        parser.run().unwrap();
        let matcher = parser.into_matcher().unwrap();
        matcher.try_match("4 5 hello").unwrap();
    }

    #[test]
    fn separator_includes_hash() {
        let matcher = FmtMatcher::new("##{num}: (#{count})", &["hi"]).unwrap();
        assert!(matcher.try_match("#5: (some)").is_ok())
    }
}
