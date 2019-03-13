//! Utilities and shared helpers.

/// Fields in a struct or enum member.
pub enum Fields<'a> {
    Unnamed(usize),
    Named(&'a [&'a str]),
}

impl<'a> From<&'a [&'a str]> for Fields<'a> {
    fn from(src: &'a [&'a str]) -> Fields {
        Fields::Named(src)
    }
}

impl<'a> From<usize> for Fields<'a> {
    fn from(src: usize) -> Fields<'static> {
        Fields::Unnamed(src)
    }
}

impl<'a> Fields<'a> {
    pub fn len(&self) -> usize {
        match self {
            Fields::Unnamed(size) => *size,
            Fields::Named(names) => names.len(),
        }
    }

    pub fn are_named(&self) -> bool {
        match self {
            Fields::Named(_) => true,
            Fields::Unnamed(_) => false,
        }
    }
}
