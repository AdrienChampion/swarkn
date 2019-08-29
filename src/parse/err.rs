//! Parse error.

use crate::prelude::*;

/// Parse error information.
///
/// The display implementation produces a pretty, multi-line representation of the error.
///
/// # Examples
///
/// ```rust
/// use swarkn::parse::err::ErrPos;
/// let pos = ErrPos::new(4, 13, "if it is the deep sea I can see you there");
/// let pretty = pos.to_string();
/// let mut lines = pretty.lines();
/// assert_eq! { lines.next(), Some("   |".into()) };
/// assert_eq! { lines.next(), Some(" 5 | if it is the deep sea I can see you there".into()) };
/// assert_eq! { lines.next(), Some("   |              ^".into()) };
/// ```
///
/// ```rust
/// use swarkn::parse::err::ErrPos;
/// let pos = ErrPos::new(728, 13, "if it is the deep sea I can see you there");
/// let pretty = pos.to_string();
/// let mut lines = pretty.lines();
/// assert_eq! { lines.next(), Some("     |".into()) };
/// assert_eq! { lines.next(), Some(" 729 | if it is the deep sea I can see you there".into()) };
/// assert_eq! { lines.next(), Some("     |              ^".into()) };
/// ```
#[derive(Debug, Clone)]
pub struct ErrPos {
    /// Row (starts at 0).
    pub row: usize,
    /// Column (starts at 0).
    pub col: usize,
    /// Line of the error (no newline).
    pub line: String,
}
impl ErrPos {
    /// Constructor.
    pub fn new<S>(row: usize, col: usize, line: S) -> Self
    where
        S: Into<String>,
    {
        let line = line.into();
        Self { row, col, line }
    }
}
impl fmt::Display for ErrPos {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let line_idx = (self.row + 1).to_string();
        writeln!(fmt, " {0: <1$} |", "", line_idx.len())?;
        writeln!(fmt, " {} | {}", line_idx, self.line)?;
        write!(fmt, " {0: <1$} | {0: >2$}^", "", line_idx.len(), self.col)
    }
}

error_chain::error_chain! {
    types {
        ParseErr, ParseErrKind, ParseResExt, ParseRes;
    }

    foreign_links {
        Regex(regex::Error);
    }

    errors {
        Parsing(pos: ErrPos, blah: String) {
            description("parse error")
            display(
                "{}, line {} column {}\n{}", blah, pos.row + 1, pos.col + 1, pos
            )
        }
    }
}

impl ParseErr {
    /// Multi-line representation of a trace of errors.
    ///
    /// See the [module-level documentation] for more.
    ///
    /// [module-level documentation]: index.html (module-level documentation)
    pub fn pretty(&self) -> String {
        let mut s = "error: ".to_string();

        // Reverse errors.
        let mut errs = vec![];
        for e in self.iter() {
            errs.push(e)
        }

        let mut is_first = true;
        for e in errs.into_iter().rev() {
            if is_first {
                is_first = false
            } else {
                s.push_str("\n")
            }
            s.push_str(&e.to_string())
        }

        s
    }
}
