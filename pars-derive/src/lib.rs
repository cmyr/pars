//! A library for deriving simple FromStr implementations.
//!
//! This library provides adds a new trait, `ParsFromStr`, which
//! functions like the `FromStr` trait in the standard library.
//!
//! This trait can be derived in two ways: either with _very_ naive
//! fmt-like pattern matching, or by specifying a regex.
//!
//! # Examples
//!
//! ## Naive pattern variant
//!
//! Given input like "home: (56.43, -13.23)"
//!
//! ```ignore
//! #[pars::fmt("#name: (#x, #y)")]
//! struct NamedPosition {
//!     name: String,
//!     x: f64,
//!     y: f64,
//! }
//! ```
//!
//! ## Regex variant
//!
//! Given input like "home: (56.43, -13.23)"
//!
//! ```ignore
//! #[pars::re(r"(\w+): \((.*), (.*)\)")]
//! struct NamedPosition {
//!     name: String,
//!     x: f64,
//!     y: f64,
//! }
//! ```
//!
//! Given input like "unit", "struct hello 5", "tuple hello 4"
//!
//! ```ignore
//! #[pars::fmt("#_: (#type)")] // sketchy af
//! #[pars::re(fn = ThreeKinds::get_type)]
//! #[pars::re(r"(\w_).*"), Uppercase]
//! enum ThreeKinds {
//!     Unit,
//!     #[pars(re(r"\w+ (\w+) (\w+)")]
//!     Struct {
//!        name: String,
//!        size: usize,
//!     },
//!     #[pars(re(r"\w+ (\w+) (\w+)")]
//!     Tuple(String, usize),
//! }
//! ```

#![recursion_limit = "256"]

extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

mod container;

use container::{Container, Data, Field, Style};
use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Span, TokenStream};
use std::vec::Vec;
use syn::{AttributeArgs, DeriveInput, Lit, NestedMeta};

enum Mode {
    Fmt(AttributeArgs),
    Regex(AttributeArgs),
}

/// Returns the format/re string, e.g. for #[pars::fmt("my string")] returns
#[proc_macro_attribute]
pub fn re(attr: TokenStream1, tokens: TokenStream1) -> TokenStream1 {
    let args = parse_macro_input!(attr as AttributeArgs);
    let mode = Mode::Regex(args);

    let item = parse_macro_input!(tokens as DeriveInput);
    let container = Container::from_ast(&item).unwrap();

    generate_impls(mode, &container).unwrap_or_else(to_compile_errors).into()
}

#[proc_macro_attribute]
pub fn fmt(attr: TokenStream1, tokens: TokenStream1) -> TokenStream1 {
    let args = parse_macro_input!(attr as AttributeArgs);
    let mode = Mode::Fmt(args);

    let item = parse_macro_input!(tokens as DeriveInput);
    let container = Container::from_ast(&item).unwrap();

    generate_impls(mode, &container).unwrap_or_else(to_compile_errors).into()
}

/// The 'main' function for the actual code generation
fn generate_impls(mode: Mode, cont: &Container) -> Result<TokenStream, Vec<syn::Error>> {
    let ident = &cont.ident;
    let orig = &cont.original;

    let mode_block = match mode {
        Mode::Regex(ref attrs) => generate_re_block(attrs, &cont)?,
        Mode::Fmt(ref attrs) => generate_fmt_block(attrs, &cont)?,
    };

    let body = match &cont.data {
        Data::Struct(Style::Struct, _) => gen_struct_body(cont),
        Data::Struct(Style::Tuple, ref fields) => gen_tuple_body(&mode, fields),
        _other => unimplemented!(),
    };

    let mut impl_block = quote! {

        #orig

        impl ::pars::ParsFromStr for #ident {
            fn pars_from_str(src: &str) -> Result<Self, ::pars::MatchError<'static>> {

                #mode_block

                Ok(
                    #ident
                    #body
                )
            }
        }
    };

    impl_block.extend(maybe_generate_from_str(ident, &mode));

    Ok(impl_block)
}

fn maybe_generate_from_str(ident: &syn::Ident, mode: &Mode) -> TokenStream {
    let attrs = match mode {
        Mode::Regex(ref attrs) | Mode::Fmt(ref attrs) => attrs,
    };

    match attrs.len() {
        1 => quote! {},
        2 => match &attrs[1] {
            syn::NestedMeta::Meta(syn::Meta::Word(extra_arg)) => {
                if extra_arg.to_string() == "gen_from_str" {
                    generate_from_str(ident)
                } else {
                    panic!("Wrong string {:?}", ident.to_string())
                }
            }
            _ => panic!("Misunderstood {:?}", &attrs[1]),
        },
        _ => panic!(
            "That number of arguments doesn't look right. \
             We only allow the one extra because parsing the \
             arguments to pars::re(whatever) is hard."
        ),
    }
}

fn generate_from_str(ident: &syn::Ident) -> TokenStream {
    quote! {
        impl ::std::str::FromStr for #ident {
            type Err = ::pars::MatchError<'static>;

            fn from_str(s: &str) -> Result<Self, ::pars::MatchError<'static>> {
                use ::pars::ParsFromStr;
                Self::pars_from_str(s)
            }
        }
    }
}

fn generate_re_block(
    attrs: &AttributeArgs,
    cont: &Container,
) -> Result<TokenStream, Vec<syn::Error>> {
    let re_string = get_attr_string(attrs).map_err(|e| vec![e])?;
    let num_fields = cont.data.num_fields();
    let field_names = cont.data.to_field_names();
    let field_names =
        field_names.as_ref().map(|v| v.iter().map(|s| s.as_str()).collect::<Vec<_>>());

    let pattern = match field_names.as_ref() {
        Some(ref names) => pars_fmt::RegexMatcher::new(&re_string, names.as_slice()),
        None => pars_fmt::RegexMatcher::new_unnamed(&re_string, num_fields),
    };

    let pattern = pattern.map_err(|e| vec![syn::Error::new(Span::call_site(), e.to_string())])?;
    let field_names = gen_static_array(&field_names.clone().unwrap_or_default());
    let position_mapping = gen_static_array(&pattern.make_position_mapping());

    let re_block = quote! {

        let position_mapping = #position_mapping;
        let field_names: &[&str] = &#field_names;
        let mut ordered_matches = [""; #num_fields];

        static INSTANCE: ::pars::OnceCell<::pars::RegexMatcher> = ::pars::OnceCell::INIT;
        let pat = INSTANCE.get_or_init(|| {
            if field_names.is_empty() {
                ::pars::RegexMatcher::new(&#re_string, #num_fields).unwrap()
            } else {
                ::pars::RegexMatcher::new(&#re_string, field_names).unwrap()
            }
        });

        let captures = pat.captures(src)?;

        for i in 0..#num_fields {
            let match_str = match captures.0.get(i + 1) {
                Some(m) => m.as_str(),
                None => return Err(pars::MatchError::MissingCaptureGroup(i)),
            };
            let match_idx = position_mapping[i];
            ordered_matches[match_idx] = match_str;
        }
    };
    Ok(re_block)
}

fn generate_fmt_block(
    attrs: &AttributeArgs,
    cont: &Container,
) -> Result<TokenStream, Vec<syn::Error>> {
    if let Data::Enum(_) = cont.data {
        panic!("pars::fmt only works with structs");
    }

    let fmt_string = get_attr_string(attrs).map_err(|e| vec![e])?;
    let num_fields = cont.data.num_fields();
    let field_names = cont.data.to_field_names().unwrap_or_default();
    let field_names = field_names.iter().map(|s| s.as_str()).collect::<Vec<_>>();

    // check that the fmt string is valid
    let matcher = ::pars_fmt::FmtMatcher::new_named(&fmt_string, field_names.as_slice())
        .map_err(|e| vec![syn::Error::new(Span::call_site(), e)])?;

    let lead_separator = matcher.get_lead_separator_str();
    let separator_indices = gen_static_array_2tuple(&matcher.make_separator_indices());

    let fmt_block = quote! {
        let lead_separator = #lead_separator;
        let separator_indices = #separator_indices;

        let mut pos = 0;
        let mut current_sep = 0;
        let mut ordered_matches = [""; #num_fields];

        match src.find(lead_separator) {
            Some(0) => pos = lead_separator.len(),
            _ => return Err(::pars::MatchError::missing_separator(current_sep, lead_separator)),
        };

        for (separator, field_idx) in &separator_indices {
            if pos == src.len() {
                return Err(::pars::MatchError::InputExhausted);
            }
            if separator.is_empty() {
                ordered_matches[*field_idx] = &src[pos..];
                pos = src.len();
                continue;
            }

            match src[pos..].find(separator) {
                Some(idx) => {
                    ordered_matches[*field_idx] = &src[pos..pos + idx];
                    pos = pos + idx + separator.len();
                }
                None => return Err(::pars::MatchError::missing_separator(current_sep, separator)),
            }
            current_sep += 1;
        }
    };
    Ok(fmt_block)
}

/// Generates tokens for a static array from a slice.
fn gen_static_array<T: quote::ToTokens>(inp: &[T]) -> TokenStream {
    let mut out = TokenStream::new();
    for item in inp {
        out.extend(quote! { #item, });
    }
    quote! { [#out] }
}

/// Generates tokens for a static array from a slice of 2tuples.
fn gen_static_array_2tuple<T1, T2>(inp: &[(T1, T2)]) -> TokenStream
where
    T1: quote::ToTokens,
    T2: quote::ToTokens,
{
    let mut out = TokenStream::new();
    for (one, two) in inp {
        out.extend(quote! { (#one, #two), });
    }
    quote! { [#out] }
}

fn gen_struct_body<'a>(cont: &Container) -> TokenStream {
    let mut out = TokenStream::new();
    for (i, field) in cont.data.all_fields().enumerate() {
        let ident = field.original.ident.as_ref().unwrap();
        let ty = field.ty;

        out.extend(quote! {
            #ident: ordered_matches[#i].parse()
            .map_err(|e| pars::MatchError::field_failed(stringify!(#ident), stringify!(#ty), ordered_matches[#i].to_string()))?,
        });
    }

    quote! {{#out}}
}

fn gen_tuple_body<'a>(_mode: &Mode, _fields: &[Field<'a>]) -> TokenStream {
    unimplemented!()
}

/// "my string".
fn get_attr_string(args: &AttributeArgs) -> Result<String, syn::Error> {
    match args.first() {
        Some(NestedMeta::Literal(Lit::Str(s))) => Ok(s.value()),
        Some(_other) => Err(syn::Error::new_spanned(_other, "first argument should be a string")),
        None => panic!("pars requires arguments"),
    }
}

fn to_compile_errors(errors: Vec<syn::Error>) -> proc_macro2::TokenStream {
    let compile_errors = errors.iter().map(syn::Error::to_compile_error);
    quote!(#(#compile_errors)*)
}
