extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use proc_macro2::{Span, TokenStream};
use std::vec::Vec;
use syn::{Ident, Lit, Meta, MetaNameValue};

#[proc_macro_derive(ParsFromStr, attributes(pars))]
pub fn pars_from_str(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast: syn::DeriveInput = syn::parse(tokens).unwrap();

    let mut pars_fmt = None;
    for option in ast.attrs.into_iter() {
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
    let pars_fmt = pars_fmt.expect("you must provide a format string attribute.");

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
}
