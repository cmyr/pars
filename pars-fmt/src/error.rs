//! Error types and related helpers

use std::fmt;
use std::ops::Range;

/// An error that can occur when parsing format strings.
/// This is a convenience; these errors will always occur at
/// compile time, and this lets us give better diagnostics.
#[doc(hidden)]
#[derive(Debug, PartialEq)]
pub struct FormatError {
    /// The format string
    source: String,
    /// The region containing the error
    span: Range<usize>,
    /// A diagnostic message to display to the user.
    msg: String,
}

impl FormatError {
    pub fn new<S1, S2>(source: S1, span: Range<usize>, msg: S2) -> Self
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        FormatError { source: source.into(), span, msg: msg.into() }
    }
}

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\n", self.msg)?;
        write!(f, "       {}\n       ", self.source)?;
        for i in 0..self.source.len() {
            let chr = if self.span.start <= i && self.span.end > i { '^' } else { ' ' };
            write!(f, "{}", chr)?;
        }
        Ok(())
    }
}

/// Errors that can occur during parsing.
#[derive(Debug)]
pub enum MatchError<'a> {
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
            MatchError::MissingSeparator { idx, string } => {
                //TODO: we could do fancy diagnostic errors here
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
