//! Parsing our format strings.
//!
//! Format strings are arbitrary strings containing 'field markers', indicating
//! the location of fields. Field markers begin with `#{` and end with `}`,
//! and contain the name of a struct field, or the index of a tuple field.
//!
//! Field markers must be separated by one or more dividing characters.
//! During parsing, we scan the input looking for these dividing strings,
//! which are discarded; the remaining subsections are associated with the
//! matching fields, and parsed based on the field type.
//!
//! # Examples
//!
//!```ignore
//! // A rect, represented as a space-delimited x, y, width, height
//! // example input: "10 11 320 104"
//!
//! #[pars::fmt("#{x} #{y} #{width} #{height}")]
//! struct Rect {
//!     x: usize,
//!     y: usize,
//!     width: usize,
//!     height: usize,
//! }
//!
//!```

type Error = &'static str;

enum State {
    Ready,
    InField,
    /// member is the start position
    InSeparator(usize),
    Finished,
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Field(String),
    Separator(String),
}

struct Parser<'a> {
    inp: &'a str,
    pos: usize,
    state: State,
    tokens: Vec<Token>,
}

impl<'a> Parser<'a> {
    fn new(inp: &'a str) -> Self {
        Parser { inp, pos: 0, state: State::Ready, tokens: Vec::new() }
    }

    fn run(&mut self) -> Result<(), Error> {
        loop {
            match self.state {
                State::Ready => self.advance()?,
                State::InField => self.take_field()?,
                State::InSeparator(start_pos) => self.take_separator(start_pos)?,
                State::Finished => break,
            }
        }
        Ok(())
    }

    fn advance(&mut self) -> Result<(), Error> {
        let (pos, next_state) = match self.inp.as_bytes().get(self.pos) {
            Some(&b'#') if self.inp.as_bytes().get(self.pos + 1) == Some(&b'{') => {
                if self.inp.as_bytes().get(self.pos + 2).is_none() {
                    return Err("expected identifer, found EOF");
                }

                if let Some(Token::Field(_)) = self.tokens.last() {
                    return Err("fields must be separated by non-field characters");
                }

                (self.pos + 2, State::InField)
            }
            Some(b'#') => (self.pos + 1, State::InSeparator(self.pos)),
            Some(_other) => (self.pos, State::InSeparator(self.pos)),
            None => (self.pos, State::Finished),
        };

        self.pos = pos;
        self.state = next_state;
        Ok(())
    }

    fn take_field(&mut self) -> Result<(), Error> {
        let field_end = self.inp.as_bytes()[self.pos..].iter().position(|b| b == &b'}');
        match field_end {
            Some(end) => {
                let token = Token::Field(self.inp[self.pos..self.pos + end].to_string());
                self.tokens.push(token);
                self.pos = self.pos + end + 1;
                self.state = State::Ready;
                Ok(())
            }
            None => Err("expected '}' found end of string"),
        }
    }

    fn take_separator(&mut self, start_pos: usize) -> Result<(), Error> {
        let end_pos = start_pos
            + self.inp.as_bytes()[self.pos..]
                .iter()
                .position(|b| b == &b'#')
                .unwrap_or(self.inp.len() - self.pos);

        let sep_string = self.inp[start_pos..end_pos].to_string();
        assert!(!sep_string.is_empty());
        if let Some(Token::Separator(ref mut existing)) = self.tokens.last_mut() {
            existing.push_str(&sep_string);
        } else {
            self.tokens.push(Token::Separator(sep_string));
        }
        self.pos = end_pos;
        self.state = State::Ready;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! fld {
        ($s:expr) => {
            Token::Field($s.into())
        };
    }

    macro_rules! sep {
        ($s:expr) => {
            Token::Separator($s.into())
        };
    }

    #[test]
    fn test_simple_parse() {
        let mut parser = Parser::new("#{x}, #{y} #{width} #{height}");
        assert_eq!(parser.run(), Ok(()));
        assert_eq!(
            parser.tokens,
            vec![
                fld!("x"),
                sep!(", "),
                fld!("y"),
                sep!(" "),
                fld!("width"),
                sep!(" "),
                fld!("height")
            ]
        );
    }

    #[test]
    fn test_fail_with_unseparated_fields() {
        let mut parser = Parser::new("#{x}#{y} #{width} #{height}");
        assert!(parser.run().is_err());
    }
}
