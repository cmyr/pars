//! Error types and related helpers

use std::fmt;

pub type Error = &'static str;

#[derive(Debug, Clone)]
pub enum MatchError<'a> {
    IncorrectFields { found: Vec<String>, expected: Vec<String> },
    MissingSeparator { idx: usize, string: &'a str },
    InputExhausted,
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
        }
    }
}
