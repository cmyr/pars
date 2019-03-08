#[allow(unused_imports)]
#[macro_use]
extern crate pars_derive;
#[doc(hidden)]
pub use pars_derive::*;
pub use pars_fmt::*;

use regex::Error as RegexErr;
/// exported as public so derived functions have access to regex.
pub use regex::Regex;
use std::str::FromStr;

pub trait ParsFromStr: Sized {
    fn pars_from_str(s: &str) -> Result<Self, Error>;
}

#[derive(Debug)]
pub enum Error {
    MatchFailed(String),
    ParseFailed(Box<::std::error::Error>),
}

/// This is a sneaky re-export so that the client crate doesn't need to have
/// the regex crate in its cargo.toml.
#[doc(hidden)]
pub fn regex_new(s: &str) -> Result<Regex, RegexErr> {
    Regex::new(s)
}

/// This is a sneaky re-export so that the client crate doesn't need to have
/// the regex crate in its cargo.toml.
#[doc(hidden)]
pub use once_cell::sync::OnceCell;

#[doc(hidden)]
pub trait Matches {
    fn parse_idx<E, T>(&self, idx: usize) -> Result<T, Error>
    where
        E: std::error::Error + Sized + 'static,
        T: FromStr<Err = E>;
}

impl<'a> Matches for regex::Captures<'a> {
    fn parse_idx<E, T>(&self, idx: usize) -> Result<T, Error>
    where
        E: std::error::Error + Sized + 'static,
        T: FromStr<Err = E>,
    {
        let s = self
            .get(idx + 1)
            .ok_or_else(|| Error::MatchFailed(format!("missing match {}", idx)))?;
        s.as_str().parse::<T>().map_err(|e| Error::ParseFailed(Box::new(e))).into()
    }
}

impl Matches for Vec<&str> {
    fn parse_idx<E, T>(&self, idx: usize) -> Result<T, Error>
    where
        E: std::error::Error + Sized + 'static,
        T: FromStr<Err = E>,
    {
        self[idx].parse::<T>().map_err(|e| Error::ParseFailed(Box::new(e)))
    }
}

impl<'a, 'b> Matches for FmtMatch<'a, 'b> {
    fn parse_idx<E, T>(&self, idx: usize) -> Result<T, Error>
    where
        E: std::error::Error + Sized + 'static,
        T: FromStr<Err = E>,
    {
        let s = self
            .get_match(idx)
            .map_err(|_| Error::MatchFailed(format!("missing match {}", idx)))?;
        s.parse::<T>().map_err(|e| Error::ParseFailed(Box::new(e)))
    }
}

#[cfg(test)]
mod tests {}
