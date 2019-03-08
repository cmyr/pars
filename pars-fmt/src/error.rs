//! Error types and related helpers

use std::fmt;
use std::ops::Range;

/// An error that can occur when parsing format strings.
/// This is a convenience; these errors will always occur at
/// compile time, and this lets us give better diagnostics.
#[doc(hidden)]
#[derive(Debug, PartialEq)]
pub struct FormatError {
    inp: String,
    span: Range<usize>,
    msg: String,
}

impl FormatError {
    pub fn new<S1, S2>(inp: S1, span: Range<usize>, msg: S2) -> Self
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        FormatError { inp: inp.into(), span, msg: msg.into() }
    }
}

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\n", self.msg)?;
        write!(f, "       {}\n       ", self.inp)?;
        for i in 0..self.inp.len() {
            let chr = if self.span.start <= i && self.span.end > i { '^' } else { ' ' };
            write!(f, "{}", chr)?;
        }
        Ok(())
    }
}

/// Errors that can occur during parsing.
#[derive(Debug)]
pub enum MatchError<'a> {
    IncorrectFields {
        found: Vec<String>,
        expected: Vec<String>,
    },
    MissingSeparator {
        idx: usize,
        string: &'a str,
    },
    InputExhausted,
    FieldFailed {
        expected_type: &'static str,
        member: &'static str,
        inner: Option<Box<dyn std::error::Error + 'static>>,
    },
}

impl<'a> MatchError<'a> {
    pub(crate) fn missing_separator(idx: usize, string: &'a str) -> Self {
        MatchError::MissingSeparator { idx, string }
    }
}

impl<'a> fmt::Display for MatchError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MatchError::InputExhausted => write!(f, "input exhausted"),
            MatchError::IncorrectFields { .. } => write!(f, "incorrect fields"),
            MatchError::MissingSeparator { idx, string } => {
                write!(f, "missing separator at index {}, text: '{}'", idx, string)
            }
            MatchError::FieldFailed { expected_type, member, inner } => {
                write!(f, "failed to parse field {}, expected {}", member, expected_type)?;
                if let Some(err) = inner {
                    write!(f, ": {}", err)?;
                }
                Ok(())
            }
        }
    }
}
