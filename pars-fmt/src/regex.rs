//! Regex backed parsing

use crate::error::FormatError;
use regex::{Captures, Error as RegexErr, Regex};

pub struct RegexMatcher {
    pattern: Regex,
    fields: Fields,
}

enum Fields {
    Unnamed(usize),
    Named(Vec<String>),
}

impl From<Vec<String>> for Fields {
    fn from(src: Vec<String>) -> Fields {
        Fields::Named(src)
    }
}

impl From<Vec<&str>> for Fields {
    fn from(src: Vec<&str>) -> Fields {
        Fields::Named(src.iter().map(|s| String::from(*s)).collect())
    }
}

impl From<usize> for Fields {
    fn from(src: usize) -> Fields {
        Fields::Unnamed(src)
    }
}

impl Fields {
    pub fn len(&self) -> usize {
        match self {
            Fields::Unnamed(size) => *size,
            Fields::Named(names) => names.len(),
        }
    }
}

impl RegexMatcher {
    fn new<T: Into<Fields>>(string: &str, fields: T) -> Result<Self, FormatError> {
        let fields = fields.into();
        let pattern = Regex::new(string)
            .map_err(|e| FormatError::new(string, 0..string.len(), e.to_string()))?;

        let capture_names = pattern.capture_names().skip(1).collect::<Vec<_>>();
        assert!(
            capture_names.len() != 0,
            "haven't thought through semantics of having no capture groups"
        );

        if capture_names.len() != fields.len() {
            return Err(FormatError::new(
                string,
                0..string.len(),
                format!(
                    "incorrect number of groups in pattern. Found {} expected {}.",
                    capture_names.len(),
                    fields.len()
                ),
            ));
        }

        if capture_names
            .iter()
            .zip(capture_names.iter().cycle().skip(1))
            .any(|(c1, c2)| c1.is_none() != c2.is_none())
        {
            return Err(FormatError::new(
                string,
                0..string.len(),
                "If using named capture groups, all groups must be named",
            ));
        }

        if let Fields::Named(ref names) = fields {
            // check for existence of names now
            for group_name in capture_names.iter().flat_map(|s| s) {
                if names.iter().position(|name| name == group_name).is_none() {
                    return Err(FormatError::new(
                        string,
                        0..string.len(),
                        format!("Unexpected capture group: \"{}\"", group_name),
                    ));
                }
            }
        }
        Ok(RegexMatcher { pattern, fields })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "no capture groups")]
    fn expect_capture_groups() {
        let _ = RegexMatcher::new(r"[abc]+ \d+", 0).unwrap();
    }

    #[test]
    fn named_group_lengths_equal() {
        let pattern = r"(?P<name>.+), (?P<count>\d+)";
        let _ = RegexMatcher::new(pattern, vec!["name", "count"]).unwrap();
    }

    #[test]
    #[should_panic(expected = "Found 2 expected 3")]
    fn named_group_lengths_unequal() {
        let pattern = r"(?P<name>.+), (?P<count>\d+)";
        let _ = RegexMatcher::new(pattern, vec!["name", "count", "friends"]).unwrap();
    }

    #[test]
    #[should_panic(expected = "all groups must be named")]
    fn disallow_mixed_named_and_unnamed_groups() {
        let pattern = r"(\d+), (?P<named>.+)";
        let _ = RegexMatcher::new(pattern, vec!["name", "count"]).unwrap();
    }

    #[test]
    fn all_named_or_unnamed_is_fine() {
        let pattern = r"(\d+), (.+)";
        let _ = RegexMatcher::new(pattern, vec!["name", "count"]).unwrap();

        let pattern = r"(\d+), (.+)";
        let _ = RegexMatcher::new(pattern, 2).unwrap();

        let pattern = r"(?P<count>\d+), (?P<name>.+)";
        let _ = RegexMatcher::new(pattern, vec!["name", "count"]).unwrap();
    }

    #[test]
    #[should_panic(expected = "Unexpected capture group: \"NO_EXIST\"")]
    fn unexpected_field_should_fail() {
        let pattern = r"(?P<count>\d+), (?P<NO_EXIST>.+)";
        let _ = RegexMatcher::new(pattern, vec!["name", "count"]).unwrap();
    }
}
