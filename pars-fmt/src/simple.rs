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

impl Token {
    fn is_field(&self) -> bool {
        match self {
            Token::Field(_) => true,
            Token::Separator(_) => false,
        }
    }
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

    /// Check that fields in the struct match up with fields in the fmt string.
    fn validate_fields(&self, fields: &[String]) -> Result<(), FormatError> {
        let num_fields = self.tokens.iter().filter(|t| t.is_field()).count();

        if fields.len() != num_fields {
            return Err(FormatError::new(
                self.source,
                0..self.source.len(),
                format!("expected {} fields, found {}", fields.len(), num_fields),
            ));
        }

        for field_range in self.tokens.iter().filter_map(|t| match t {
            Token::Separator(_) => None,
            Token::Field(ref range) => Some(range),
        }) {
            let field = &self.source[field_range.clone()];
            if fields.iter().find(|s| *s == field).is_none() {
                return Err(FormatError::new(
                    self.source,
                    field_range.clone(),
                    format!("unexpected field, expected one of {}", fields.join(", ")),
                ));
            }
        }
        Ok(())
    }

    fn into_matcher(self, fields: Vec<String>) -> Result<FmtMatcher<'a>, FormatError> {
        self.validate_fields(&fields)?;
        let Parser { tokens, source, .. } = self;
        let lead_separator = match tokens.first() {
            Some(Token::Separator(s)) => Some(s.clone()),
            _ => None,
        };

        let mut fmt_fields = Vec::with_capacity(fields.len());
        let skip = if lead_separator.is_some() { 1 } else { 0 };
        let mut iter = tokens.into_iter().skip(skip);
        loop {
            match (iter.next(), iter.next()) {
                (Some(Token::Field(field)), Some(Token::Separator(sep))) => {
                    fmt_fields.push((field, sep))
                }
                (Some(Token::Field(field)), None) => {
                    fmt_fields.push((field, source.len()..source.len()))
                }
                (Some(Token::Field(range)), _) | (Some(Token::Separator(range)), _) => {
                    return Err(FormatError::new(source, range, "unexpected token"));
                }
                (None, None) => break,
                (None, Some(_)) => unreachable!(),
            }
        }

        Ok(FmtMatcher { source, lead_separator, fmt_fields, fields })
    }

    #[cfg(test)]
    fn resolve_tokens(&'a self) -> Vec<&'a str> {
        self.tokens
            .iter()
            .map(|t| match t {
                Token::Field(range) => &self.source[range.clone()],
                Token::Separator(range) => &self.source[range.clone()],
            })
            .collect()
    }
}

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
    pub fn new<S: AsRef<str>>(fmt_string: &'a str, fields: &[S]) -> Result<Self, FormatError> {
        let mut parser = Parser::new(fmt_string);
        parser.run()?;
        //TODO: we should be able to have this be an array or something
        let fields = fields.iter().map(|s| s.as_ref().to_string()).collect::<Vec<_>>();
        parser.into_matcher(fields)
    }

    pub fn get_lead_separator_str(&'a self) -> &'a str {
        self.lead_separator.clone().map(|r| &self.source[r]).unwrap_or("")
    }

    /// Creates a vector of ordered separator strings and the index,
    /// into the ordered field names, of the item corresponding to that separator.
    pub fn make_separator_indices(&'a self) -> Vec<(&'a str, usize)> {
        let mut result = Vec::new();
        for (field, sep) in self.fmt_fields.iter() {
            let sep_string = &self.source[sep.clone()];
            let field_str = &self.source[field.clone()];
            let field_idx = self
                .fields
                .iter()
                .position(|f| f == field_str)
                .expect("all fields have been validated");
            result.push((sep_string, field_idx));
        }
        result
    }

    pub fn try_match<'b>(&'a self, source: &'b str) -> Result<FmtMatch<'a, 'b>, MatchError<'a>> {
        // we insert the field locations in the order they appear in the struct declaration,
        // so we need to have a vec we can just index into.
        let mut values = vec![0..0; self.fmt_fields.len()];
        // current position in the source string.
        let mut pos = 0;
        // we track this for better diagnostics.
        let mut current_sep = 0;

        if let Some(ref head) = self.lead_separator {
            let sep_string = &self.source[head.clone()];
            match source.find(sep_string) {
                Some(0) => pos = head.len(),
                _ => return Err(MatchError::missing_separator(current_sep, sep_string)),
            }
            current_sep += 1;
        }

        let separators = self.make_separator_indices();
        for (sep_string, field_idx) in separators {
            if pos == source.len() {
                return Err(MatchError::InputExhausted);
            }
            if sep_string.is_empty() {
                // take all remaining string
                values[field_idx] = pos..source.len();
                pos = source.len();
                continue;
            }

            match &source[pos..].find(sep_string) {
                Some(sep_start) => {
                    values[field_idx] = pos..pos + sep_start;
                    pos = pos + sep_start + sep_string.len();
                }
                None => return Err(MatchError::missing_separator(current_sep, sep_string)),
            }

            current_sep += 1;
        }

        Ok(FmtMatch { matcher: self, source, values })
    }
}

impl<'a, 'b> FmtMatch<'a, 'b> {
    pub fn get(&'a self, idx: usize) -> &'a str {
        let range = self.values.get(idx).expect("all indices should be validated");
        &self.source[range.clone()]
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
        let fields = ["x", "y", "width", "height"].into_iter().map(|s| String::from(*s)).collect();
        parser.run().unwrap();
        let matcher = parser.into_matcher(fields).unwrap();
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
        let fields = ["x", "y", "width", "height"].into_iter().map(|s| String::from(*s)).collect();
        let matcher = parser.into_matcher(fields).unwrap();
        matcher.try_match("4 5 hello").unwrap();
    }

    #[test]
    fn separator_includes_hash() {
        let matcher = FmtMatcher::new("##{num}: (#{count})", &["num", "count"]).unwrap();
        assert!(matcher.try_match("#5: (some)").is_ok())
    }

    #[test]
    #[should_panic(expected = "expected 3 fields")]
    fn missing_field_should_fail() {
        let fmt_str = "#{x} #{y}";
        let fields = &["x", "y", "z"];
        let _ = FmtMatcher::new(fmt_str, fields).unwrap();
    }

    #[test]
    #[should_panic(expected = "expected 1 fields")]
    fn extra_field_should_fail() {
        let fmt_str = "#{x} #{y}";
        let fields = &["x"];
        let _ = FmtMatcher::new(fmt_str, fields).unwrap();
    }

    #[test]
    #[should_panic(expected = "unexpected field")]
    fn unexpected_field_should_fail() {
        let fmt_str = "#{x} #{y}";
        let fields = &["x", "NO"];
        let _ = FmtMatcher::new(fmt_str, fields).unwrap();
    }

    #[test]
    fn fields_out_of_order() {
        let fmt_str = "#{x} #{y}";
        let fields = &["y", "x"];
        let matcher = FmtMatcher::new(fmt_str, fields).unwrap();

        let mtch = matcher.try_match("hi mom").unwrap();
        assert_eq!(mtch.get(0), "mom");
        assert_eq!(mtch.get(1), "hi");
    }
}
