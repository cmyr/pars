//! Implementation for pars functionality. This lives in its own crate
//! to avoid circular dependencies.

mod error;
mod simple;

pub use simple::{FmtMatch, FmtMatcher};
pub use error::{Error, MatchError};
