#[allow(unused_imports)]
#[macro_use]
extern crate pars_derive;
#[doc(hidden)]
pub use pars_derive::*;

use regex::{Regex, Error as RegexErr};

pub trait ParsFromStr: Sized {
    type Err;
    fn pars_from_str(s: &str) -> Result<Self, Self::Err>;
}

/// This is a sneaky re-export so that the client crate doesn't need to have
/// the regex crate in it's cargo.toml.
#[doc(hidden)]
pub fn regex_new(s: &str) -> Result<Regex, RegexErr> {
    Regex::new(s)
}

#[cfg(test)]
mod tests {
}
