//! # Pars
//!
//! Pars automates parsing simple structured strings into Rust structs.
//!
//! This is intended to provide functionality akin to the built in
//! `FromStr` trait.
//!
//! The main type in this crate is the [`ParsFromStr`] trait, which
//! should not be implemented directly; it is automatically generated
//! by the procedural macros described below.
//!
//! ## Use
//!
//! The intended use-case is in parsing data from some custom string
//! representation. For instance, imagine you have a list of rectangles
//! that have been serialized with the format, "Rect((0, 3), (20, 22))"
//! where the first tuple is the origin and the second is the size.
//!
//! ```
//! use pars::ParsFromStr;
//!
//! #[pars::fmt("Rect((#{x}, #{y}), (#{width}, #{height}))")]
//! struct Rect {
//!     x: u32,
//!     y: u32,
//!     width: u32,
//!     height: u32,
//! }
//!
//! let Rect { x, y, width, height } = Rect::pars_from_str("Rect((0, 3), (20, 22))").unwrap();
//! assert_eq!((x, y, width, height), (0, 3, 20, 22));
//! ```
//!
//! ## Format strings
//!
//! The best way to use Pars is with the built-in format notation. This uses
//! `#{}` to indicate the presence of a field. Fields can be named or unnamed;
//! if they are unnamed they will be applied in the order they are declared
//! in the target struct.
//!
//! This format langauge is essentially a way of indicating regions of the input
//! format that delineate fields. As such it is **very** limited:
//!
//! - All fields must be separated by non-field characters ("#{name}#{age}"
//! will not compile)
//! - input must have an unambiguous structure. For instance,
//! ("names: Colin, Smriti, Casimir blue" cannot be parsed by
//! "names: #{names} #{color}", because the space after `#{names}` will
//! match the space after 'Colin,'.
//!
//! Pars is intended to be **lightweight**, **barebones**, and **fast**. In many
//! use cases, the code generated by Pars will be as fast as when written by
//! hand.
//!
//! Field names can be out of order:
//!
//!```
//! use pars::ParsFromStr;
//!
//! #[pars::fmt("(#{x}, #{y}): #{name}")]
//! struct NamedPosition {
//!     name: String,
//!     x: f64,
//!     y: f64,
//! }
//!
//! let example = "(42.4, -555.111): Home";
//! let NamedPosition { name, x, y } = NamedPosition::pars_from_str(example).unwrap();
//! assert_eq!((name.as_str(), x, y), ("Home", 42.4, -555.111));
//!```
//!
//! Field names are ignored on tuple structs:
//!
//!```
//! use pars::ParsFromStr;
//!
//! #[pars::fmt("(#{x}, #{y})")]
//! struct Point(f64, f64);
//!
//! let example = "(42.4, -5)";
//! let point = Point::pars_from_str(example).unwrap();
//! assert_eq!((point.0, point.1), (42.4, -5.));
//!```
//!
//! Or can be left out:
//!
//!```
//! use pars::ParsFromStr;
//!
//! #[pars::fmt("(#{_}, #{_})")]
//! struct Point(f64, f64);
//!
//! let example = "(42.4, -5)";
//! let point = Point::pars_from_str(example).unwrap();
//! assert_eq!((point.0, point.1), (42.4, -5.));
//!```
//!
//! ## Regex
//!
//! In situations where Pars' format is inadequate, you may use regex instead:
//!
//!```
//! use pars::ParsFromStr;
//!
//! let sample_input = ["colin daniels 55", "casimir faithful ndolo 4"];
//!
//!#[pars::re(r"([\w ]+) (\d+)")]
//! struct Person {
//!     name: String,
//!     rank: u32,
//! }
//!
//! let Person { name, rank } = Person::pars_from_str(&sample_input[0]).unwrap();
//! assert_eq!((name.as_str(), rank), ("colin daniels", 55));
//!
//! let Person { name, rank } = Person::pars_from_str(&sample_input[1]).unwrap();
//! assert_eq!((name.as_str(), rank), ("casimir faithful ndolo", 4));
//!```

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
