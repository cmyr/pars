//! Implementation for pars functionality. This lives in its own crate
//! to avoid circular dependencies.

mod error;
mod simple;

pub use error::{FormatError, MatchError};
pub use simple::{FmtMatch, FmtMatcher};
