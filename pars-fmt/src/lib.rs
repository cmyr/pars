//! Implementation for pars functionality. This lives in its own crate
//! to avoid circular dependencies.

mod common;
mod error;
mod regex;
mod simple;

pub use crate::regex::{RegexMatch, RegexMatcher};
pub use error::{FormatError, MatchError};
pub use simple::{FmtMatch, FmtMatcher};
