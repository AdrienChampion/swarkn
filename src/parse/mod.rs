//! A very barebone zero-copy parser.
//!
//! This parser is designed to parse relatively simple languages. The main feature it provides is
//! simple yet helpful error-reporting. The main type of this module is [`Parser`], and it comes
//! with the [`ParserExt`] trait.
//!
//! Since this parser is zero-copy and works on `&'txt str`, both [`Parser`] and [`ParserExt`] take
//! a lifetime usually called `'txt`, which will appear quite a lot here. It is the lifetime of the
//! text being parsed, which is stored inside [`Parser`].
//!
//! [`Parser`] is the actual opaque parser: you cannot change its state directly for safety reasons.
//! It provides helpers to build the parser you dream of, which are in the [`ParserExt`] trait.
//! Implementing this trait consists in writing two accessors for the actual [`Parser`]: one
//! immutable and one mutable. [`ParserExt`] then provides everything automatically.
//!
//! # Workflow
//!
//! The intended workflow is to wrap a [`Parser`] into your own `struct`, say `MyParser`, and then
//! implement [`ParserExt`] for [`MyParser`]. Typically,
//!
//! ```rust
//! use swarkn::parse::*;
//!
//! /// A custom parser.
//! pub struct MyParser<'txt> {
//!     /// Actual parser.
//!     pub parser: Parser<'txt>,
//! }
//! impl<'txt> ParserExt<'txt> for MyParser<'txt> {
//!     fn parser(&self) -> &Parser<'txt> {
//!         &self.parser
//!     }
//!     fn parser_mut(&mut self) -> &mut Parser<'txt> {
//!         &mut self.parser
//!     }
//! }
//! ```
//!
//! We can now leverage [`ParserExt`]'s helpers to implement the higher-level parsing functions we
//! need. Say we want Rust-style comments, and we (wisely) decide to leverage the [`ws`]
//! (whitespace), [`sl_cmt`] (single-line comment) and [`ml_cmt`] (multi-line comment) functions of
//! [`ParserExt`]:
//!
//! ```rust
//! # use swarkn::parse::*;
//! # /// A custom parser.
//! # pub struct MyParser<'txt> {
//! #     /// Actual parser.
//! #     pub parser: Parser<'txt>,
//! # }
//! # impl<'txt> ParserExt<'txt> for MyParser<'txt> {
//! #     fn parser(&self) -> &Parser<'txt> {
//! #         &self.parser
//! #     }
//! #     fn parser_mut(&mut self) -> &mut Parser<'txt> {
//! #         &mut self.parser
//! #     }
//! # }
//! impl<'txt> MyParser<'txt> {
//!     /// Parses a sequence of whitespace-separated comments.
//!     ///
//!     /// - parses single-line `// ... \n` comments;
//!     /// - parses multi-line `/* ... */` comments;
//!     pub fn cmt(&mut self) {
//!         let mut start = self.pos();
//!         loop {
//!             self.ws();
//!             self.sl_cmt("//");
//!             self.ws();
//!             self.ml_cmt("/*", "*/");
//!             let current = self.pos();
//!             debug_assert!(start <= current);
//!             if start == current {
//!                 break;
//!             } else {
//!                 start = current
//!             }
//!         }
//!     }
//! }
//! ```
//!
//! For a bigger (although not very big) example, refer to the [`example`] sub-module.
//!
//! # The [`ParserErrorExt`] trait
//!
//! This trait provides two error-handling functions:
//!
//! - [`error`], to fail at the current position, and
//! - [`error_at`], to fail at a specific position.
//!
//! These functions live in a separate trait so that users can chose to bring them in scope or not.
//!
//! [`Parser`]: struct.Parser.html (The Parser struct)
//! [`ParserExt`]: trait.ParserExt.html (The ParserExt trait)
//! [`MyParser`]: ./example/struct.MyParser.html (An example of using Parser and ParserExt)
//! [`ws`]: struct.Parser.html#method.ws (The ws function for Parser)
//! [`sl_cmt`]: struct.Parser.html#method.sl_cmt (The sl_cmt function for Parser)
//! [`ml_cmt`]: struct.Parser.html#method.ml_cmt (The ml_cmt function for Parser)
//! [`example`]: ./example/index.html (An example of using Parser and ParserExt)
//! [`ParserErrorExt`]: trait.ParserErrorExt.html (The ParserErrorExt trait)
//! [`error`]: trait.ParserErrorExt.html#method.error (The error function for ParserErrorExt)
//! [`error_at`]: trait.ParserErrorExt.html#method.error_at
//! (The error_at function for ParserErrorExt)

pub use regex;
pub use regex::Regex;

use crate::prelude::*;

pub mod err;
pub mod example;

pub use err::{ParseRes, ParseResExt};

/// Private helpers.
mod helpers {
    use super::*;

    /// Tries to construct a regex.
    pub fn new_regex<Re>(regex: Re) -> ParseRes<Regex>
    where
        Re: AsRef<str>,
    {
        match Regex::new(regex.as_ref()) {
            Ok(regex) => Ok(regex),
            Err(e) => {
                let err: err::ParseErr = e.into();
                bail!(err.chain_err(|| "while constructing regex from raw string"))
            }
        }
    }
}

/// A position in the parser.
///
/// Internally, a position is just a cursor.
///
/// ```rust
/// use swarkn::parse::Pos;
/// assert_eq!(std::mem::size_of::<Pos>(), std::mem::size_of::<usize>())
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
    /// Actual position in the text.
    cursor: usize,
}
impl Pos {
    /// Constructor.
    fn new(cursor: usize) -> Self {
        Self { cursor }
    }
}

/// A barebone zero-copy parser.
pub struct Parser<'txt> {
    /// Text we're parsing.
    text: &'txt str,
    /// Position in the text.
    cursor: usize,
}
impl<'txt> Parser<'txt> {
    /// Constructor.
    pub fn new(text: &'txt str) -> Self {
        Self { text, cursor: 0 }
    }
}
impl<'txt> ParserExt<'txt> for Parser<'txt> {
    fn parser(&self) -> &Parser<'txt> {
        self
    }
    fn parser_mut(&mut self) -> &mut Parser<'txt> {
        self
    }
}

/// Trait providing basic parsing operations.
pub trait ParserExt<'txt> {
    /// Parser accessor.
    fn parser(&self) -> &Parser<'txt>;

    /// Parser accessor (mutable).
    fn parser_mut(&mut self) -> &mut Parser<'txt>;

    /// The current position.
    fn pos(&self) -> Pos {
        Pos::new(self.parser().cursor)
    }

    /// Backtracks to a previous position.
    ///
    /// The position must preceed the current position.
    fn backtrack(&mut self, pos: Pos) {
        debug_assert!(self.pos() >= pos);
        self.parser_mut().cursor = pos.cursor
    }

    /// Line the cursor is currently at.
    ///
    /// - if the cursor points to a line feed, the line before the line feed is returned;
    /// - assumes line feeds are encoded as `'\n'`;
    /// - panics if the cursor is in an illegal position.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("if it is the deep sea\nI can see you there");
    ///
    /// ```
    fn pos_line(&self) -> &'txt str {
        let parser = self.parser();
        let input_len = parser.text.len();

        // If the text is empty, return it.
        if input_len == 0 {
            return parser.text;
        }

        let (mut start, mut end) = (parser.cursor, parser.cursor);

        // Find the first newline before the cursor.
        for char in parser.text[0..start].chars().rev() {
            if char == '\n' {
                break;
            }
            debug_assert!(start > 0);
            start -= 1
        }

        // Find the first newline after the cursor.
        for char in parser.text[end..].chars().rev() {
            debug_assert!(end <= input_len);
            end += 1;
            if char == '\n' {
                break;
            }
        }

        &parser.text[start..end]
    }

    /// Rest of the input from the current position.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new(" \tblah");
    /// assert_eq!(parser.rest(), " \tblah");
    /// parser.ws();
    /// assert_eq!(parser.rest(), "blah");
    /// ```
    fn rest(&self) -> &'txt str {
        let parser = self.parser();
        &parser.text[parser.cursor..]
    }

    /// True if there is no more text to parse.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let parser = Parser::new("");
    /// assert! { parser.is_eoi() }
    /// ```
    fn is_eoi(&self) -> bool {
        self.parser().cursor >= self.parser().text.len()
    }

    /// Parses some characters until the stop function is true (exclusive).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("772032 0003 blah");
    /// assert_eq!(parser.chars_until(|char| !char.is_numeric()), "772032");
    /// parser.ws();
    /// assert_eq!(parser.rest(), "0003 blah");
    /// ```
    fn chars_until<Stop>(&mut self, mut stop: Stop) -> &'txt str
    where
        Stop: FnMut(char) -> bool,
    {
        let mut offset = 0;
        for char in self.rest().chars() {
            if stop(char) {
                break;
            } else {
                offset += 1
            }
        }
        let parser = self.parser_mut();
        let start = parser.cursor;
        let end = start + offset;
        parser.cursor = end;
        &parser.text[start..end]
    }

    /// Parses some characters until the stop function is true (inclusive).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("772032 0003 blah");
    /// assert_eq!(parser.chars_until_including(|char| !char.is_numeric()), "772032 ");
    /// parser.ws();
    /// assert_eq!(parser.rest(), "0003 blah");
    /// ```
    fn chars_until_including<Stop>(&mut self, mut stop: Stop) -> &'txt str
    where
        Stop: FnMut(char) -> bool,
    {
        let mut offset = 0;
        for char in self.rest().chars() {
            offset += 1;
            if stop(char) {
                break;
            }
        }
        let parser = self.parser_mut();
        let start = parser.cursor;
        let end = start + offset;
        parser.cursor = end;
        &parser.text[start..end]
    }

    /// Parses whitespace characters.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new(" \n\t     blah");
    /// parser.ws();
    /// assert_eq!(parser.rest(), "blah");
    /// ```
    fn ws(&mut self) {
        self.chars_until(|char| !char.is_whitespace());
        ()
    }

    /// Parses a single-line comment starting with some tag.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("#! /bin/bash\nexit 0");
    /// parser.sl_cmt("#");
    /// assert_eq!(parser.rest(), "exit 0");
    /// ```
    fn sl_cmt<Str>(&mut self, tag: Str)
    where
        Str: AsRef<str>,
    {
        if self.try_tag(tag) {
            self.chars_until_including(|c| c == '\n');
            ()
        }
    }
    /// Parses a multi-line comment starting/ending with some tag.
    ///
    /// - start/end tags might be equal;
    /// - supports nested comments, except if start/end tags are equal (for obvious reasons);
    /// - panics if either tag is empty;
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("<| \nfancy <||| nested \n|||> comments |> with a tail");
    /// parser.ml_cmt("<|", "|>");
    /// assert_eq!(parser.rest(), " with a tail");
    /// ```
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// // Same start/end tag? no problem.
    /// let mut parser = Parser::new("| \nfancy | not nested | comments | with a tail");
    /// parser.ml_cmt("|", "|");
    /// assert_eq!(parser.rest(), " not nested | comments | with a tail");
    /// ```
    fn ml_cmt<Str1, Str2>(&mut self, start_tag: Str1, end_tag: Str2)
    where
        Str1: AsRef<str>,
        Str2: AsRef<str>,
    {
        let start_tag = start_tag.as_ref();
        let end_tag = end_tag.as_ref();
        if self.try_tag(start_tag) {
            let start_first_char = if let Some(char) = start_tag.chars().next() {
                char
            } else {
                panic!("[swarkn] illegal empty start tag for a multi-line comment")
            };
            let end_first_char = if let Some(char) = end_tag.chars().next() {
                char
            } else {
                panic!("[swarkn] illegal empty end tag for a multi-line comment")
            };

            let (mut start, mut end) = (1, 0);

            loop {
                self.chars_until(|char| char == start_first_char || char == end_first_char);
                if self.try_tag(end_tag) {
                    end += 1
                } else if self.try_tag(start_tag) {
                    start += 1
                } else {
                    self.parser_mut().cursor += 1;
                }
                if start == end {
                    break;
                }
            }
        }
    }

    /// Tries to parse a regular expression.
    ///
    /// This parser will reject any regex match that does not start from the current position.
    /// Hence, you should **always** start your regex's with `^`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("726 i_heartz_catz, me_too");
    /// // Oh no. The next regex does not start with `^`.
    /// let ident_1 = Regex::new(r#"[a-zA-Z_]+"#).unwrap();
    /// // No match anyways.
    /// assert_eq!(parser.try_regex(&ident_1), None);
    /// // This one's better.
    /// let ident_2 = Regex::new(r#"^[a-zA-Z_]+"#).unwrap();
    /// // Still no match obviously.
    /// assert_eq!(parser.try_regex(&ident_2), None);
    ///
    /// assert_eq!(parser.digits().unwrap(), "726");
    /// parser.ws();
    /// assert_eq!(parser.rest(), "i_heartz_catz, me_too");
    ///
    /// let ident_start = parser.pos();
    /// assert_eq!(parser.try_regex(&ident_1).unwrap(), "i_heartz_catz");
    /// assert_eq!(parser.rest(), ", me_too");
    ///
    /// parser.backtrack(ident_start);
    /// assert_eq!(parser.try_regex(&ident_2).unwrap(), "i_heartz_catz");
    /// assert_eq!(parser.rest(), ", me_too");
    /// ```
    fn try_regex(&mut self, regex: &Regex) -> Option<&'txt str> {
        let parser = self.parser_mut();
        if let Some(res) = regex.find(&parser.text[parser.cursor..]) {
            if res.start() == 0 {
                let res = res.as_str();
                parser.cursor += res.len();
                return Some(res);
            }
        }
        None
    }

    /// Parses a regular expression or fails.
    ///
    /// This parser will reject any regex match that does not start from the current position.
    /// Hence, you should **always** start your regex's with `^`.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("726 i_heartz_catz, me_too");
    /// // Oh no. The next regex does not start with `^`.
    /// let ident_1 = Regex::new(r#"[a-zA-Z_]+"#).unwrap();
    /// // No match anyways.
    /// assert_eq!(
    ///     parser.regex(&ident_1).unwrap_err().to_string(),
    ///     "\
    /// expected a match for regular expression r#\"[a-zA-Z_]+\"#, line 1 column 1
    ///    |
    ///  1 | 726 i_heartz_catz, me_too
    ///    | ^\
    ///     "
    /// );
    /// // This one's better.
    /// let ident_2 = Regex::new(r#"^[a-zA-Z_]+"#).unwrap();
    /// // Still no match obviously.
    /// assert_eq!(
    ///     parser.regex(&ident_2).unwrap_err().to_string(),
    ///     "\
    /// expected a match for regular expression r#\"^[a-zA-Z_]+\"#, line 1 column 1
    ///    |
    ///  1 | 726 i_heartz_catz, me_too
    ///    | ^\
    ///     "
    /// );
    ///
    /// assert_eq!(parser.digits().unwrap(), "726");
    /// parser.ws();
    /// assert_eq!(parser.rest(), "i_heartz_catz, me_too");
    ///
    /// let ident_start = parser.pos();
    /// assert_eq!(parser.regex(&ident_1).unwrap(), "i_heartz_catz");
    /// assert_eq!(parser.rest(), ", me_too");
    ///
    /// parser.backtrack(ident_start);
    /// assert_eq!(parser.regex(&ident_2).unwrap(), "i_heartz_catz");
    /// assert_eq!(parser.rest(), ", me_too");
    /// ```
    fn regex(&mut self, regex: &Regex) -> ParseRes<&'txt str>
    where
        Self: ParserErrorExt<'txt>,
    {
        if let Some(res) = self.try_regex(regex) {
            Ok(res)
        } else {
            bail!(self.error(format!(
                "expected a match for regular expression r#\"{}\"#",
                regex
            )))
        }
    }

    /// Tries to parse a regex built from a string.
    ///
    /// - fails if the string is not a valid regex;
    fn try_raw_regex<Str>(&mut self, regex: Str) -> ParseRes<Option<&'txt str>>
    where
        Str: AsRef<str>,
    {
        let regex = helpers::new_regex(regex)?;
        Ok(self.try_regex(&regex))
    }

    /// Parses a regex built from a string or fails.
    ///
    /// - fails also if the string is not a valid regex;
    fn raw_regex<Str>(&mut self, regex: Str) -> ParseRes<&'txt str>
    where
        Self: ParserErrorExt<'txt>,
        Str: AsRef<str>,
    {
        let regex = helpers::new_regex(regex)?;
        self.regex(&regex)
    }

    /// Tries to parse a tag (a string).
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("blah42");
    /// assert! { !parser.try_tag("doesn't match") }
    /// assert_eq!(parser.rest(), "blah42");
    /// assert! { parser.try_tag("blah") }
    /// assert_eq!(parser.rest(), "42");
    /// ```
    fn try_tag<Str>(&mut self, tag: Str) -> bool
    where
        Str: AsRef<str>,
    {
        let tag = tag.as_ref();
        let parser = self.parser_mut();
        let end = parser.cursor + tag.len();
        if end <= parser.text.len() {
            if &parser.text[parser.cursor..end] == tag {
                parser.cursor += tag.len();
                return true;
            }
        }
        false
    }

    /// Parses a tag (a string) or fails.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("blah42_");
    /// assert! { parser.tag("doesn't match").is_err() }
    /// assert_eq!(parser.rest(), "blah42_");
    /// assert! { parser.tag("blah").is_ok() }
    /// assert_eq!(parser.rest(), "42_");
    /// assert! { parser.tag("42").is_ok() }
    /// assert! { parser.tag("_").is_ok() }
    /// assert_eq!(parser.rest(), "");
    /// ```
    fn tag<Str>(&mut self, tag: Str) -> ParseRes<()>
    where
        Self: ParserErrorExt<'txt>,
        Str: AsRef<str>,
    {
        let tag = tag.as_ref();
        if self.try_tag(tag) {
            Ok(())
        } else {
            bail!(self.error(format!("expected `{}`", tag)))
        }
    }

    /// Tries to parse some digits.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("772032 0003 blah");
    /// assert_eq!(parser.try_digits().unwrap(), "772032");
    /// parser.ws();
    /// assert_eq!(parser.try_digits().unwrap(), "0003");
    /// assert_eq!(parser.rest(), " blah");
    /// assert_eq!(parser.try_digits(), None);
    /// ```
    fn try_digits(&mut self) -> Option<&'txt str> {
        let res = self.chars_until(|char| !char.is_numeric());
        if res.is_empty() {
            None
        } else {
            Some(res)
        }
    }

    /// Parses some digits or fails.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("772032 0003 blah");
    /// assert_eq!(parser.digits().unwrap(), "772032");
    /// parser.ws();
    /// assert_eq!(parser.try_digits().unwrap(), "0003");
    /// parser.ws();
    /// assert_eq!(parser.rest(), "blah");
    /// assert_eq!(
    ///     parser.digits().unwrap_err().pretty(),
    ///     "\
    /// error: expected digits, line 1 column 13
    ///    |
    ///  1 | 772032 0003 blah
    ///    |             ^\
    ///     "
    /// );
    /// ```
    fn digits(&mut self) -> ParseRes<&'txt str>
    where
        Self: ParserErrorExt<'txt>,
    {
        if let Some(digits) = self.try_digits() {
            Ok(digits)
        } else {
            bail!(self.error("expected digits"))
        }
    }

    /// Tries to parse a usize.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("772032 0003 blah");
    /// assert_eq!(parser.try_usize().unwrap(), 772_032);
    /// parser.ws();
    /// assert_eq!(parser.try_usize().unwrap(), 3);
    /// assert_eq!(parser.rest(), " blah");
    /// assert_eq!(parser.try_usize(), None);
    /// ```
    fn try_usize(&mut self) -> Option<usize> {
        let start_pos = self.pos();
        if let Some(digits) = self.try_digits() {
            use std::str::FromStr;
            if let Ok(usize) = usize::from_str(digits) {
                return Some(usize);
            }
        }
        self.backtrack(start_pos);
        None
    }

    /// Parses a usize or fails.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("772032 0003 blah");
    /// assert_eq!(parser.usize().unwrap(), 772_032);
    /// parser.ws();
    /// assert_eq!(parser.usize().unwrap(), 3);
    /// parser.ws();
    /// assert_eq!(parser.rest(), "blah");
    /// assert_eq!(
    ///     parser.usize().unwrap_err().pretty(),
    ///     "\
    /// error: expected an integer (usize), line 1 column 13
    ///    |
    ///  1 | 772032 0003 blah
    ///    |             ^\
    ///     "
    /// );
    /// ```
    fn usize(&mut self) -> ParseRes<usize>
    where
        Self: ParserErrorExt<'txt>,
    {
        if let Some(int) = self.try_usize() {
            Ok(int)
        } else {
            bail!(self.error("expected an integer (usize)"))
        }
    }

    /// Retrieves the row, column, and line of a position.
    fn position_details(&self, pos: Pos) -> err::ErrPos {
        let Pos { cursor: pos } = pos;
        let (mut row, mut count, mut col_line) = (0, 0, None);

        for line in self.parser().text.lines() {
            debug_assert! { pos >= count }

            // Is the position in the current line, or at the end of the current line?
            if pos <= count + line.len() + 1 {
                col_line = Some((pos - count, line.to_string()));
                break;
            } else {
                // Position is not in the current line, move on.
                row += 1;
                count += line.len() + 1
            }
        }

        if let Some((col, line)) = col_line {
            err::ErrPos::new(row, col, line)
        } else {
            panic!("could not find position `{}` in input text, while retrieving position details")
        }
    }
}

impl<'txt, T> ParserErrorExt<'txt> for T where T: ParserExt<'txt> {}

/// Error-handling functions.
///
/// These functions live in a different trait because one might not want to bring them in scope.
pub trait ParserErrorExt<'txt>: ParserExt<'txt> {
    /// Reports an error at the current position.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("772032 0003 blah");
    /// assert_eq!(parser.usize().unwrap(), 772_032);
    /// parser.ws();
    /// assert_eq!(
    ///     parser.error(r#"I hate leading zeros >_<"#).pretty(),
    ///     "\
    /// error: I hate leading zeros >_<, line 1 column 8
    ///    |
    ///  1 | 772032 0003 blah
    ///    |        ^\
    ///     "
    /// )
    /// ```
    fn error<S: Into<String>>(&self, blah: S) -> err::ParseErr {
        self.error_at(self.pos(), blah)
    }

    /// Reports an error at some position.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use swarkn::parse::*;
    /// let mut parser = Parser::new("772032 0003 blah");
    /// assert_eq!(parser.usize().unwrap(), 772_032);
    /// parser.ws();
    /// let stupid_leading_zeros = parser.pos();
    /// assert_eq!(parser.usize().unwrap(), 3);
    /// parser.ws();
    /// parser.tag("blah").unwrap();
    /// assert!(parser.is_eoi());
    ///
    /// assert_eq!(
    ///     parser.error_at(stupid_leading_zeros, "I hate leading zeros too (>.<)").pretty(),
    ///     "\
    /// error: I hate leading zeros too (>.<), line 1 column 8
    ///    |
    ///  1 | 772032 0003 blah
    ///    |        ^\
    ///     "
    /// )
    /// ```
    fn error_at<S: Into<String>>(&self, pos: Pos, blah: S) -> err::ParseErr {
        let position = self.position_details(pos);
        err::ParseErrKind::Parsing(position, blah.into()).into()
    }
}
