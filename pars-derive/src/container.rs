// The contents of this file are adapted from serde
// (https://serde.rs) and are used under the terms of
// the MIT license, reproduced below:
//
//
// Permission is hereby granted, free of charge, to any
// person obtaining a copy of this software and associated
// documentation files (the "Software"), to deal in the
// Software without restriction, including without
// limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of
// the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice
// shall be included in all copies or substantial portions
// of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
// ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
// TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT
// SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

use proc_macro::TokenStream;
use syn;
use syn::punctuated::Punctuated;

type Attrs = Vec<TokenStream>;

/// A source data structure parsed into an internal representation.
pub struct Container<'a> {
    /// The struct or enum name (without generics).
    pub ident: syn::Ident,
    /// Attributes on the structure, parsed for Serde.
    pub attrs: Attrs,
    /// The contents of the struct or enum.
    pub data: Data<'a>,
    /// Any generics on the struct or enum.
    pub generics: &'a syn::Generics,
    /// Original input.
    pub original: &'a syn::DeriveInput,
}

/// The fields of a struct or enum.
///
/// Analagous to `syn::Data`.
pub enum Data<'a> {
    Enum(Vec<Variant<'a>>),
    Struct(Style, Vec<Field<'a>>),
}

/// A variant of an enum.
pub struct Variant<'a> {
    pub ident: syn::Ident,
    pub attrs: Attrs,
    pub style: Style,
    pub fields: Vec<Field<'a>>,
    pub original: &'a syn::Variant,
}

/// A field of a struct.
pub struct Field<'a> {
    pub member: syn::Member,
    pub attrs: Attrs,
    pub ty: &'a syn::Type,
    pub original: &'a syn::Field,
}

#[derive(Copy, Clone)]
pub enum Style {
    /// Named fields.
    Struct,
    /// Many unnamed fields.
    Tuple,
    /// One unnamed field.
    Newtype,
    /// No fields.
    Unit,
}

impl<'a> Container<'a> {
    /// Convert the raw Syn ast into a parsed container object, collecting errors in `cx`.
    pub fn from_ast(item: &'a syn::DeriveInput) -> Option<Container<'a>> {
        //let mut attrs = attr::Container::from_ast(cx, item);
        let attrs = Attrs::new();

        let data = match item.data {
            syn::Data::Enum(ref data) => Data::Enum(enum_from_ast(&data.variants)),
            syn::Data::Struct(ref data) => {
                let (style, fields) = struct_from_ast(&data.fields, None);
                Data::Struct(style, fields)
            }
            syn::Data::Union(_) => {
                //cx.error_spanned_by(item, "Serde does not support derive for unions");
                panic!("no unions please");
                //                return None;
            }
        };

        let item = Container {
            ident: item.ident.clone(),
            attrs,
            data,
            generics: &item.generics,
            original: item,
        };
        //check::check(cx, &item, derive);
        Some(item)
    }
}

impl<'a> Data<'a> {
    pub fn all_fields(&'a self) -> Box<Iterator<Item = &'a Field<'a>> + 'a> {
        match *self {
            Data::Enum(ref variants) => {
                Box::new(variants.iter().flat_map(|variant| variant.fields.iter()))
            }
            Data::Struct(_, ref fields) => Box::new(fields.iter()),
        }
    }

    pub fn num_fields(&'a self) -> usize {
        match *self {
            Data::Enum(ref variants) => variants.len(),
            Data::Struct(_, ref fields) => fields.len(),
        }
    }

    pub fn to_field_names(&self) -> Option<Vec<String>> {
        let names = self
            .all_fields()
            .filter_map(|f| match &f.member {
                syn::Member::Named(ident) => Some(ident.to_string()),
                _ => None,
            })
            .collect::<Vec<_>>();
        if names.is_empty() {
            None
        } else {
            Some(names)
        }
    }
}

fn enum_from_ast<'a>(
    variants: &'a Punctuated<syn::Variant, Token![,]> // container_default: &attr::Default
) -> Vec<Variant<'a>> {
    variants
        .iter()
        .map(|variant| {
            //            let attrs = attr::Variant::from_ast(cx, variant);
            let attrs = Attrs::new();
            let (style, fields) = struct_from_ast(&variant.fields, Some(&attrs));
            Variant { ident: variant.ident.clone(), attrs, style, fields, original: variant }
        })
        .collect()
}

fn struct_from_ast<'a>(
    fields: &'a syn::Fields,
    attrs: Option<&Attrs>,
    // container_default: &attr::Default,
) -> (Style, Vec<Field<'a>>) {
    match *fields {
        syn::Fields::Named(ref fields) => (Style::Struct, fields_from_ast(&fields.named, attrs)),
        syn::Fields::Unnamed(ref fields) if fields.unnamed.len() == 1 => {
            (Style::Newtype, fields_from_ast(&fields.unnamed, attrs))
        }
        syn::Fields::Unnamed(ref fields) => (Style::Tuple, fields_from_ast(&fields.unnamed, attrs)),
        syn::Fields::Unit => (Style::Unit, Vec::new()),
    }
}

fn fields_from_ast<'a>(
    fields: &'a Punctuated<syn::Field, Token![,]>,
    _attrs: Option<&Attrs>,
    // container_default: &attr::Default,
) -> Vec<Field<'a>> {
    fields
        .iter()
        .enumerate()
        .map(|(i, field)| Field {
            member: match field.ident {
                Some(ref ident) => syn::Member::Named(ident.clone()),
                None => syn::Member::Unnamed(i.into()),
            },
            //            attrs: attr::Field::from_ast(cx, i, field, attrs, container_default),
            attrs: Attrs::new(),
            ty: &field.ty,
            original: field,
        })
        .collect()
}
