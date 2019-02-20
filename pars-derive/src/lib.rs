extern crate proc_macro;
extern crate syn;
#[macro_use] extern crate quote;

use proc_macro::TokenStream;
use syn::{Lit, Meta, MetaNameValue};

#[proc_macro_derive(ParsFromStr, attributes(pars))]
pub fn pars_from_str(tokens: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(tokens).unwrap();
    
    let mut pars_fmt = None;
    for option in ast.attrs.into_iter() {
        let option = option.parse_meta().unwrap();
        match option {
            // Match `#[ident = lit]` attributes.  Match guard makes it `#[prefix = lit]`
            Meta::NameValue(MetaNameValue{ref ident, ref lit, ..}) if ident == "pars" => {
                if let Lit::Str(lit) = lit {
                    pars_fmt = Some(lit.value());
                } // else return some type error
            }
            _ => (),
            // other => eprintln!("other attr {:?}", other),
        }
    }
    let pars_fmt = pars_fmt.unwrap();
    
    let gen = quote! {
        fn our_fun_function() -> &'static str {
            
            #pars_fmt
        }
    };
    gen.into()
}




// 
// impl std::str::FromStr for NamedPos {
//     type Err = String;
//     fn from_str(s: &str) -> Result<Self, String> {
//         let mut iter = s.split_whitespace();
//         let one = iter.next().unwrap();
//         let two = iter.next().unwrap();
//         let three = iter.next().unwrap();
//         
//         name = one.chars().take_while(|c| c != ':').collect::<String>();
//         let two_parsed = two[1..].parse::<isize>().map_err(String::from("failed to parse one"))?;
        // etc
//     }
// }
