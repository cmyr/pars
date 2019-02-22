#[allow(unused_imports)]
#[macro_use]
extern crate pars_derive;
#[doc(hidden)]
pub use pars_derive::*;

pub trait ParsFromStr: Sized {
    type Err;
    fn pars_from_str(s: &str) -> Result<Self, Self::Err>;
}

#[cfg(test)]
mod tests {
}
