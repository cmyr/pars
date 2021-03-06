//! Regex backed parsing

use crate::common::Fields;
use crate::error::{FormatError, MatchError};
use regex::{Captures, Regex};

/// A compiled regex pattern, and related metadata.
#[doc(hidden)]
pub struct RegexMatcher<'a> {
    source: &'a str,
    pattern: Regex,
    fields: Fields<'a>,
    pat_has_names: bool,
}

/// The result of a successful match.
///
/// The second field is the original source string. This is hacky; we hold
/// on to it so that we have a valid lifetime when constructing `MatchError`s.
#[doc(hidden)]
pub struct RegexMatch<'a, 'b>(pub Captures<'b>, &'a str);

impl<'a> RegexMatcher<'a> {
    pub fn new_unnamed(string: &'a str, num_fields: usize) -> Result<Self, FormatError> {
        let fields = Fields::Unnamed(num_fields);
        Self::new(string, fields)
    }

    pub fn new<T: Into<Fields<'a>>>(string: &'a str, fields: T) -> Result<Self, FormatError> {
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

        let pat_has_names = capture_names[0].is_some();

        if pat_has_names {
            if let Fields::Named(ref names) = fields {
                // check for existence of names now
                for group_name in capture_names.iter().map(|n| n.unwrap()) {
                    if names.iter().position(|name| *name == group_name).is_none() {
                        return Err(FormatError::new(
                            string,
                            0..string.len(),
                            format!("Unexpected capture group: \"{}\"", group_name),
                        ));
                    }
                }
            } else {
                return Err(FormatError::new(
                    string,
                    0..string.len(),
                    "group names meaningless for type without field names",
                ));
            }
        }
        Ok(RegexMatcher { pattern, fields, source: string, pat_has_names })
    }

    /// returns a vec mapping the position of captures in the regex to
    /// the position of fields in the struct.
    pub fn make_position_mapping(&self) -> Vec<usize> {
        match (&self.fields, self.pat_has_names) {
            (Fields::Named(names), true) => self
                .pattern
                .capture_names()
                .skip(1)
                .map(|n| {
                    let name = n.expect("name missing: input should be validated");
                    names
                        .iter()
                        .position(|field_name| *field_name == name)
                        .expect("input should be validated")
                })
                .collect(),
            (fields, false) => (0..fields.len()).collect(),
            _ => unreachable!(),
        }
    }

    pub fn captures<'b>(&'a self, input: &'b str) -> Result<RegexMatch<'a, 'b>, MatchError<'a>> {
        let captures = self.pattern.captures(input).ok_or(MatchError::MatchFailed)?;
        Ok(RegexMatch(captures, &self.source))
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
        let fields: &[&str] = &["name", "count"];
        let _ = RegexMatcher::new(pattern, fields).unwrap();
    }

    #[test]
    #[should_panic(expected = "Found 2 expected 3")]
    fn named_group_lengths_unequal() {
        let pattern = r"(?P<name>.+), (?P<count>\d+)";
        let fields: &[&str] = &["name", "count", "friends"];
        let _ = RegexMatcher::new(pattern, fields).unwrap();
    }

    #[test]
    #[should_panic(expected = "all groups must be named")]
    fn disallow_mixed_named_and_unnamed_groups() {
        let pattern = r"(\d+), (?P<named>.+)";
        let fields: &[&str] = &["name", "count"];
        let _ = RegexMatcher::new(pattern, fields).unwrap();
    }

    #[test]
    fn all_named_or_unnamed_is_fine() {
        let pattern = r"(\d+), (.+)";
        let fields: &[&str] = &["name", "count"];
        let _ = RegexMatcher::new(pattern, fields).unwrap();

        let pattern = r"(\d+), (.+)";
        let _ = RegexMatcher::new(pattern, 2).unwrap();

        let pattern = r"(?P<count>\d+), (?P<name>.+)";
        let _ = RegexMatcher::new(pattern, fields).unwrap();
    }

    #[test]
    #[should_panic(expected = "Unexpected capture group: \"NO_EXIST\"")]
    fn unexpected_field_should_fail() {
        let pattern = r"(?P<count>\d+), (?P<NO_EXIST>.+)";
        let fields: &[&str] = &["name", "count"];
        let _ = RegexMatcher::new(pattern, fields).unwrap();
    }
}
