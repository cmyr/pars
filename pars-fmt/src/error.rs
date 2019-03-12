//! Error types and related helpers

use std::fmt;
use std::ops::Range;

/// An error that can occur when parsing format strings.
/// This is a convenience; these errors will always occur at
/// compile time, and this lets us give better diagnostics.
#[doc(hidden)]
#[derive(PartialEq)]
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

impl fmt::Debug for FormatError {
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

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Failed to parse \"{}\": {} ({}..{})",
            self.source, self.msg, self.span.start, self.span.end
        )
    }
}

/// Errors that can occur during parsing.
#[derive(Debug)]
pub enum MatchError<'a> {
    MatchFailed,
    MissingSeparator { idx: usize, string: &'a str },
    InputExhausted,
    FieldFailed { member: &'static str, expected_type: &'static str, input: String },
}

impl<'a> MatchError<'a> {
    pub(crate) fn missing_separator(idx: usize, string: &'a str) -> Self {
        MatchError::MissingSeparator { idx, string }
    }

    pub fn field_failed(member: &'static str, expected_type: &'static str, input: String) -> Self {
        MatchError::FieldFailed { expected_type, member, input }
    }
}

impl<'a> fmt::Display for MatchError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MatchError::MatchFailed => write!(f, "match failed"),
            MatchError::InputExhausted => write!(f, "input exhausted"),
            MatchError::MissingSeparator { idx, string } => {
                //TODO: we could do fancy diagnostic errors here
                write!(f, "missing separator at index {}, text: '{}'", idx, string)
            }
            MatchError::FieldFailed { member, expected_type, input } => {
                write!(f, "failed to parse {}: {} from \"{}\"", member, expected_type, input)
            }
        }
    }
}
