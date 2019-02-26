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

extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

mod container;

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Span, TokenStream};
use std::vec::Vec;
use std::collections::HashMap;
use syn::{DeriveInput, Ident, Item, ItemStruct, ItemEnum, Lit, Meta, MetaNameValue, NestedMeta, AttributeArgs};
use container::{Container, Data, Field};

#[proc_macro_attribute]
pub fn re(attr: TokenStream1, tokens: TokenStream1) -> TokenStream1 {
    //ensure_no_extra_attrs(&ast.attrs);
    let args = parse_macro_input!(attr as AttributeArgs);
    let re_string = get_attr_string(args);

    let parse_tokens = tokens.clone();
    let item = parse_macro_input!(parse_tokens as DeriveInput);
    let container = Container::from_ast(&item).unwrap();

    let generated = match &container.data {
        Data::Enum(_) => unimplemented!(),
        Data::Struct(_,_) => gen_re_struct(re_string, &container),
    };

    tokens.into_iter().chain(generated).collect()
}

//FIXME: static regex or something, at least don't be compiling in the parse fn
fn gen_re_struct(re_string: String, container: &Container) -> TokenStream1 {
    let parse_prelude = quote! {
        let pat = ::pars::regex_new(#re_string).unwrap();
        let caps = pat.captures(s).ok_or("failed to match input")?;
    };

    let struct_name = container.ident.clone();

    let parse_body : TokenStream =
        container.data.all_fields()
        .enumerate()
        .map(|(i, field)| {
            let field_name = field.original.ident
                .clone()
                .expect("named struct fields only sowwwy");

            quote! {
                let #field_name = caps.get(#i + 1)
                    .map(|s| s.as_str().parse().map_err(|_| "parse failed"))
                    .ok_or("missing field")??;
            }
        })
    .collect();

    let parse_body = quote!{
        #parse_prelude
        #parse_body
    };

    gen_parse_fn(struct_name, parse_body, container.data.all_fields()).into()
}

fn gen_parse_fn<'a, I: Iterator<Item = &'a Field<'a>>>(struct_name: Ident, parse_body: TokenStream, fields: I) -> TokenStream {
    let initializer_body: TokenStream = fields 
        .map(|field| {
            let field_name = field.original.ident
                .clone()
                .expect("named struct fields only sowwwy");

            quote!{
                #field_name : #field_name,
            }
        }).collect();

    quote! {
        impl #struct_name {
            fn pars_from_str(s: &str) -> ::std::result::Result<#struct_name, &'static str> {
                #parse_body

                Ok(
                    #struct_name {
                        #initializer_body
                    }
                )
            }
        }
    }
}

fn gen_re_enum(re_string: String, container: &Container) -> TokenStream1 {
    unimplemented!("enum not supported");
}

#[proc_macro_attribute]
pub fn fmt(_attr: TokenStream1, _tokens: TokenStream1) -> TokenStream1 {
    //eprintln!("fmt attr: {:?}", &attr);
    //eprintln!("fmt tokens: {:?}", &tokens);

    //let ast: syn::DeriveInput = syn::parse(tokens).unwrap();
    //ensure_no_extra_attrs(&ast.attrs);
    //eprintln!("token attrs: {:?}", ast.attrs);
    TokenStream1::new()
}

//TODO: Fancy panics with the span of 'a'
// FIXME: not called currently
fn ensure_no_extra_attrs(attrs: &[syn::Attribute]) {
    attrs.iter().for_each(|a| {
        if let Some(seg) = a.path.segments.first() {
            if seg.into_value().ident == Ident::new("pars", Span::call_site()) {
                panic!("Only one pars:: macro attribute may be used for a \
                        given type.");
            }
        }
    })
}

/// Returns the format/re string, e.g. for #[pars::fmt("my string")] returns
/// "my string".
fn get_attr_string(args: AttributeArgs) -> String {
    match args.first() {
        Some(NestedMeta::Literal(Lit::Str(s))) => s.value(),
        Some(_) => panic!("missing attr string"),
        None => panic!("pars requires arguments"),
    }
}

#[proc_macro_derive(ParsFromStr, attributes(pars))]
pub fn pars_from_str(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = syn::parse(tokens).unwrap();
    let pars_fmt = match extract_meta2(&ast) {
        Ok(Mode::Fmt(s)) => s,
        Ok(Mode::Regex { .. }) => {
            eprintln!("pars::re doesn't actually do anything yet");
            return proc_macro::TokenStream::new();
        }
        Err(e) => panic!("{}", e),
    };

    let (variables, nonvariables) = parse_vars(&pars_fmt);
    let struct_ident = ast.ident;

    let function_body = {
        let mut function_body = TokenStream::new();
        assert_eq!(nonvariables.len(), variables.len() + 1);

        let mut nonvariables = nonvariables.iter();

        for variable in &variables {
            let nonvariable = nonvariables.next().unwrap();
            let variable = Ident::new(variable, Span::call_site());

            function_body.extend(quote! {
                assert!(string.starts_with(#nonvariable));
                let (_, string) = string.split_at(#nonvariable.len());
                let split_index = string.find(|c: char| !c.is_alphanumeric())
                    .unwrap_or(string.len());
                let (#variable, string) = string.split_at(split_index);
            });
        }

        let last_nonvariable = nonvariables.next();
        function_body.extend(quote! {
            assert_eq!(string, #last_nonvariable);
        });

        function_body
    };

    let struct_initializer_body = {
        let mut struct_initializer_body = TokenStream::new();
        for variable in variables {
            let variable = Ident::new(variable, Span::call_site());
            struct_initializer_body.extend(quote! {
                #variable : #variable.parse().unwrap(),
            });
        }

        struct_initializer_body
    };

    let gen = quote! {
        fn our_fun_function(string: &str) -> #struct_ident {

            #function_body

            #struct_ident{
                #struct_initializer_body
            }
        }
    };
    gen.into()
}

/// Macro operating modes, parsed from macro attributes.
enum Mode {
    Fmt(String),
    Regex { pattern: String, group_order: Option<Vec<String>> },
}

const META_RE: &str = "pars::re";
const META_FMT: &str = "pars::fmt";

fn extract_meta2(ast: &syn::DeriveInput) -> Result<Mode, String> {
    let mut all_args = ast.attrs.iter()
        .inspect(|a| eprintln!("{:?}", a.path))
        .flat_map(|a| a.parse_meta())
        .flat_map(|meta_attr| match meta_attr {
            Meta::List(l) => {
                eprintln!("list {:?}", l);
                None
            }
            Meta::Word(w) => {
                eprintln!("word {:?}", w);
                None
            }
            Meta::NameValue(MetaNameValue {ident, lit, ..}) => {
                eprintln!("nameval {:?}: {:?}", &ident, &lit );
                Some((ident.to_string(), lit))
            }
        })
    .collect::<HashMap<_,_>>();
    println!("all args: {:?}", &all_args.keys().collect::<Vec<_>>());

    if all_args.contains_key(META_FMT) && all_args.contains_key(META_RE) {
        Err("Only one of #[pars::fmt] or #[pars::re] can be provided.".into())
    } else if let Some(lit) = all_args.remove(META_RE) {
        match lit {
            Lit::Str(s) => Ok(Mode::Regex { pattern: s.value(), group_order: None }),
            _other => Err("pars::re failed, expected str found... something else".into()),
        }
    } else if let Some(lit) = all_args.remove(META_FMT) {
        match lit {
            Lit::Str(s) => Ok(Mode::Fmt(s.value())),
            _other => Err("pars::fmt failed, expected str found... something else".into()),
        }
    } else {
        Err("`#[derive(ParsFromStr)]` requires one of `#[pars::fmt]` or #[pars::re].".into())
    }
}

/// Returns
fn extract_meta(ast: &syn::DeriveInput) -> Option<String> {
    let mut pars_fmt = None;
    for option in ast.attrs.iter() {
        let option = option.parse_meta().unwrap();
        match option {
            // Match `#[ident = lit]` attributes.  Match guard makes it `#[prefix = lit]`
            Meta::NameValue(MetaNameValue {
                ref ident, ref lit, ..
            }) if ident == "pars" => {
                if let Lit::Str(lit) = lit {
                    pars_fmt = Some(lit.value());
                } // else return some type error
            }
            _ => (),
            // other => eprintln!("other attr {:?}", other),
        }
    }

    pars_fmt
}

/// Returns (variable names, nonvariable strings).
///
/// There will always be one more nonvariables than variables (one at the beginning and one at the
/// end of the string). They may be empty.
///
/// Example CSV-ish could be
/// ```ignore
/// assert_eq!(parse_vars("$name, $date, $favorite_color"),
///            (vec!["name","date","favorite_color"],vec!["",", ",", ",""]));
/// ```
fn parse_vars(s: &str) -> (Vec<&str>, Vec<&str>) {
    let mut iter = s.split('$');

    let (mut variables, mut nonvariables) = (Vec::new(), Vec::new());

    let initial_nonvariable = iter.next().expect("nonempty pars format");
    nonvariables.push(initial_nonvariable);

    for s in iter {
        let variable_name_end = s.find(|c: char| !c.is_alphanumeric());
        let (variable_name, nonvariable) = variable_name_end
            .map(|idx| s.split_at(idx))
            // None means we're at end of string or another variable immediately follows (eg
            // "$a$b") (likely source of user error? in that case) todo maybe
            .unwrap_or((s, &""));

        variables.push(variable_name);
        nonvariables.push(nonvariable);
    }

    (variables, nonvariables)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_vars_empty() {
        assert_eq!(parse_vars(""), (vec![], vec![""]));
    }

    #[test]
    fn parse_vars_single() {
        assert_eq!(parse_vars("$a"), (vec!["a"], vec!["", ""]));
    }

    #[test]
    fn parse_vars_two_following() {
        assert_eq!(parse_vars("$a$b"), (vec!["a", "b"], vec!["", "", ""]));
    }

    #[test]
    fn parse_vars_two_space_separated() {
        assert_eq!(parse_vars("$a $b"), (vec!["a", "b"], vec!["", " ", ""]));
    }

    #[test]
    fn parse_vars_complexish() {
        assert_eq!(
            parse_vars("$name& $date* $a:$b"),
            (
                vec!["name", "date", "a", "b"],
                vec!["", "& ", "* ", ":", ""]
            )
        );
    }

    #[test]
    #[ignore]
    fn extract_meta_simple() {
        let ast = syn::parse(
            quote! {
                #[pars = "hi"]
                struct foo {}
            }
            .into(),
        )
        .unwrap();
        assert_eq!(Some("hi".into()), extract_meta(&ast));
    }
}
