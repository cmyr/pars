#[allow(unused_imports)]
#[macro_use]
extern crate pars_derive;
#[doc(hidden)]
pub use pars_derive::*;
pub use pars_fmt::*;

pub trait ParsFromStr: Sized {
    fn pars_from_str(s: &str) -> Result<Self, MatchError<'static>>;
}

//TODO: move this into internals?
/// This is a sneaky re-export so that the client crate doesn't need to have
/// this dependency explicitly.
#[doc(hidden)]
pub use once_cell::sync::OnceCell;
