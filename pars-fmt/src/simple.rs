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

use crate::common::Fields;
use crate::error::{FormatError, MatchError};

/// State, internal to the parser
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

/// A simple state-based parser for our custom format syntax.
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
    fn validate_fields(&self, fields: &Fields) -> Result<(), FormatError> {
        let num_fields = self.tokens.iter().filter(|t| t.is_field()).count();

        if fields.len() != num_fields {
            return Err(FormatError::new(
                self.source,
                0..self.source.len(),
                format!("expected {} fields, found {}", fields.len(), num_fields),
            ));
        }

        match fields {
            Fields::Named(ref names) => self.validate_field_names(names),
            Fields::Unnamed(_) => Ok(()),
        }
    }

    fn validate_field_names(&self, names: &[&str]) -> Result<(), FormatError> {
        for field_range in self.tokens.iter().filter_map(|t| match t {
            Token::Separator(_) => None,
            Token::Field(ref range) => Some(range),
        }) {
            let field = &self.source[field_range.clone()];
            if names.iter().find(|s| **s == field).is_none() {
                return Err(FormatError::new(
                    self.source,
                    field_range.clone(),
                    format!("unexpected field, expected one of {}", names.join(", ")),
                ));
            }
        }
        Ok(())
    }

    fn into_matcher(self, fields: Fields<'a>) -> Result<FmtMatcher<'a>, FormatError> {
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

/// A parsed format string and information needed to match it against
/// a given struct.
#[doc(hidden)]
pub struct FmtMatcher<'a> {
    source: &'a str,
    lead_separator: Option<Range<usize>>,
    //NOTE: we represent all substrings as ranges of self.source, to avoid
    //any unnecessary allocation.
    /// Each tuple represents the name of a field and its delineating separator.
    fmt_fields: Vec<(Range<usize>, Range<usize>)>,
    /// The fields of the destination struct.
    fields: Fields<'a>,
}

#[doc(hidden)]
#[allow(dead_code)]
pub struct FmtMatch<'a, 'b> {
    matcher: &'a FmtMatcher<'a>,
    source: &'b str,
    values: Vec<Range<usize>>,
}

impl<'a> FmtMatcher<'a> {
    pub fn new_named(fmt_string: &'a str, fields: &'a [&'a str]) -> Result<Self, FormatError> {
        let fields = Fields::Named(fields);
        Self::new(fmt_string, fields)
    }

    pub fn new_unnamed(fmt_string: &'a str, fields: usize) -> Result<Self, FormatError> {
        let fields = Fields::Unnamed(fields);
        Self::new(fmt_string, fields)
    }

    fn new(fmt_string: &'a str, fields: Fields<'a>) -> Result<Self, FormatError> {
        let mut parser = Parser::new(fmt_string);
        parser.run()?;
        parser.into_matcher(fields)
    }

    pub fn get_lead_separator_str(&'a self) -> &'a str {
        self.lead_separator.clone().map(|r| &self.source[r]).unwrap_or("")
    }

    /// Creates a vector of ordered separator strings and the index,
    /// into the ordered field names, of the item corresponding to that separator.
    pub fn make_separator_indices(&'a self) -> Vec<(&'a str, usize)> {
        let mut result = Vec::new();
        for (i, (field, sep)) in self.fmt_fields.iter().enumerate() {
            let sep_string = &self.source[sep.clone()];
            let field_str = &self.source[field.clone()];
            let field_idx = match self.fields {
                Fields::Unnamed(_) => i,
                Fields::Named(ref names) => names
                    .iter()
                    .position(|f| *f == field_str)
                    .expect("all fields have been validated"),
            };
            result.push((sep_string, field_idx));
        }
        result
    }

    #[cfg(test)]
    fn ordered_matches<'inp>(&'a self, input: &'inp str) -> Result<Vec<&'inp str>, MatchError<'a>> {
        let separator_indices = self.make_separator_indices();
        let lead_separator = self.get_lead_separator_str();
        let mut ordered_matches = vec![""; self.fields.len()];
        order_matches(input, &lead_separator, &separator_indices, &mut ordered_matches)?;
        Ok(ordered_matches)
    }
}

/// This is used at compile time to order our matches based on their names,
/// if necessary. It lives here so that when we write tests, we know they're
/// using the same logic as the macro.
#[doc(hidden)]
#[inline]
pub fn order_matches<'inp, 'data>(
    input: &'inp str,
    lead_separator: &'data str,
    separator_indices: &[(&'data str, usize)],
    ordered_matches: &mut [&'inp str],
) -> Result<(), MatchError<'data>> {
    let mut pos = 0;
    let mut current_sep = 0;

    match input.find(lead_separator) {
        Some(0) if !lead_separator.is_empty() => {
            pos = lead_separator.len();
            current_sep += 1;
        }
        Some(_) => (),
        _ => return Err(MatchError::missing_separator(current_sep, lead_separator)),
    };

    for (separator, field_idx) in separator_indices {
        if pos == input.len() {
            return Err(MatchError::InputExhausted);
        }
        if separator.is_empty() {
            ordered_matches[*field_idx] = &input[pos..];
            pos = input.len();
            continue;
        }

        match input[pos..].find(separator) {
            Some(idx) => {
                ordered_matches[*field_idx] = &input[pos..pos + idx];
                pos = pos + idx + separator.len();
            }
            None => return Err(MatchError::missing_separator(current_sep, separator)),
        }
        current_sep += 1;
    }
    Ok(())
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
        let fields = Fields::Named(&["x", "y", "width", "height"]);
        parser.run().unwrap();
        let matcher = parser.into_matcher(fields).unwrap();
        assert!(matcher.lead_separator.is_none());
        assert_eq!(matcher.fmt_fields.len(), 4);

        let amatch = matcher.ordered_matches("4, 5 10 11").unwrap();
        assert_eq!(amatch, vec!["4", "5", "10", "11"]);
    }

    #[test]
    #[should_panic(expected = "MissingSeparator")]
    fn match_should_fail() {
        let mut parser = Parser::new("#{x}, #{y} #{width} #{height}");
        parser.run().unwrap();
        let fields = Fields::Named(&["x", "y", "width", "height"]);
        let matcher = parser.into_matcher(fields).unwrap();
        matcher.ordered_matches("4 5 hello").unwrap();
    }

    #[test]
    fn separator_includes_hash() {
        let matcher = FmtMatcher::new_named("##{num}: (#{count})", &["num", "count"]).unwrap();
        assert!(matcher.ordered_matches("#5: (some)").is_ok())
    }

    #[test]
    #[should_panic(expected = "expected 3 fields")]
    fn missing_field_should_fail() {
        let fmt_str = "#{x} #{y}";
        let fields = &["x", "y", "z"];
        let _ = FmtMatcher::new_named(fmt_str, fields).unwrap();
    }

    #[test]
    #[should_panic(expected = "expected 1 fields")]
    fn extra_field_should_fail() {
        let fmt_str = "#{x} #{y}";
        let fields = &["x"];
        let _ = FmtMatcher::new_named(fmt_str, fields).unwrap();
    }

    #[test]
    #[should_panic(expected = "unexpected field")]
    fn unexpected_field_should_fail() {
        let fmt_str = "#{x} #{y}";
        let fields = &["x", "NO"];
        let _ = FmtMatcher::new_named(fmt_str, fields).unwrap();
    }

    #[test]
    fn fields_out_of_order() {
        let fmt_str = "#{x} #{y}";
        let fields = &["y", "x"];
        let matcher = FmtMatcher::new_named(fmt_str, fields).unwrap();

        let mtch = matcher.ordered_matches("hi mom").unwrap();
        assert_eq!(mtch[0], "mom");
        assert_eq!(mtch[1], "hi");
    }
}
