//! Parser example.
//!
//! This is a very tiny example that parses arithmetic expressions with variables. The full code is
//! less than 200 lines long, including the types for the AST. The parser also handles
//!
//! - single-line `// ... \n` comments;
//! - multi-line `/* ... */` comments.
//!
//! ```rust
//! use swarkn::{parse::*, parse::example::*};
//! let text ="\
//! // let's start with a single-line comment
//! ( 7 + some_value /** there's actually a `-` comming */ -
//! /* I knew it. /* I'm so good. */ */ 42) /
//! 23 /* My parser won't even fall for this: # */ * seven";
//!
//! let mut parser = MyParser::new(text);
//! assert_eq!(
//!     parser.ast().unwrap().to_string(),
//!     "( 7 + some_value - 42 ) / 23 * seven"
//! );
//! ```

use crate::parse::*;

/// An operator of our AST.
#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}
impl Op {
    /// Tries to parse an operator.
    pub fn parse(parser: &mut MyParser) -> Option<Self> {
        use Op::*;
        let op = if parser.try_tag("+") {
            Add
        } else if parser.try_tag("-") {
            Sub
        } else if parser.try_tag("*") {
            Mul
        } else if parser.try_tag("/") {
            Div
        } else {
            return None;
        };
        Some(op)
    }
}
impl fmt::Display for Op {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use Op::*;
        match self {
            Add => write!(fmt, "+"),
            Sub => write!(fmt, "-"),
            Mul => write!(fmt, "*"),
            Div => write!(fmt, "/"),
        }
    }
}

/// A token.
#[derive(Debug, Clone)]
pub enum Token<'txt> {
    Op(Op),
    Id(&'txt str),
    Nat(usize),
    OParen,
    CParen,
}
impl<'txt> fmt::Display for Token<'txt> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;
        match self {
            Op(op) => op.fmt(fmt),
            Id(id) => id.fmt(fmt),
            Nat(n) => n.fmt(fmt),
            OParen => write!(fmt, "("),
            CParen => write!(fmt, ")"),
        }
    }
}
impl<'txt> Token<'txt> {
    /// Parses a token.
    pub fn parse(parser: &mut MyParser<'txt>) -> ParseRes<Self> {
        let token = if parser.try_tag("(") {
            Token::OParen
        } else if parser.try_tag(")") {
            Token::CParen
        } else if let Some(n) = parser.try_usize() {
            Token::Nat(n)
        } else if let Some(id) = parser.try_ident() {
            Token::Id(id)
        } else if let Some(op) = parser.try_op() {
            Token::Op(op)
        } else {
            bail!(parser.error("unexpected token"))
        };
        Ok(token)
    }
}

/// Our AST.
#[derive(Debug, Clone)]
pub struct Ast<'txt> {
    pub tokens: Vec<Token<'txt>>,
}
impl<'txt> fmt::Display for Ast<'txt> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut is_first = true;
        for token in &self.tokens {
            if is_first {
                is_first = false
            } else {
                write!(fmt, " ")?
            }
            token.fmt(fmt)?
        }
        Ok(())
    }
}
impl<'txt> std::ops::Deref for Ast<'txt> {
    type Target = Vec<Token<'txt>>;
    fn deref(&self) -> &Vec<Token<'txt>> {
        &self.tokens
    }
}
impl<'txt> std::ops::DerefMut for Ast<'txt> {
    fn deref_mut(&mut self) -> &mut Vec<Token<'txt>> {
        &mut self.tokens
    }
}
impl<'txt> Ast<'txt> {
    /// Constructor.
    pub fn new() -> Self {
        Self { tokens: vec![] }
    }
}

/// A custom parser.
pub struct MyParser<'txt> {
    /// Actual parser.
    pub parser: Parser<'txt>,
}
impl<'txt> ParserExt<'txt> for MyParser<'txt> {
    fn parser(&self) -> &Parser<'txt> {
        &self.parser
    }
    fn parser_mut(&mut self) -> &mut Parser<'txt> {
        &mut self.parser
    }
}

impl<'txt> MyParser<'txt> {
    /// Constructor.
    pub fn new(txt: &'txt str) -> Self {
        Self {
            parser: Parser::new(txt),
        }
    }

    /// Parses a sequence of whitespace-separated comments.
    ///
    /// - parses single-line `// ... \n` comments;
    /// - parses multi-line `/* ... */` comments;
    pub fn cmt(&mut self) {
        let mut start = self.pos();
        loop {
            self.ws();
            self.sl_cmt("//");
            self.ws();
            self.ml_cmt("/*", "*/");
            let current = self.pos();
            debug_assert!(start <= current);
            if start == current {
                break;
            } else {
                start = current
            }
        }
    }

    /// Parses an identifier.
    pub fn try_ident(&mut self) -> Option<&'txt str> {
        self.try_raw_regex(r#"^[a-zA-Z_][a-zA-Z_0-9]*"#)
            .expect(r#"my regex is wrong /(T_T)\"#)
    }

    /// Parses an operator.
    pub fn try_op(&mut self) -> Option<Op> {
        Op::parse(self)
    }

    /// Parses a token.
    pub fn token(&mut self) -> ParseRes<Token<'txt>> {
        Token::parse(self)
    }

    /// Parses an expression.
    pub fn ast(&mut self) -> ParseRes<Ast<'txt>> {
        let mut ast = Ast::new();
        self.cmt();
        while !self.is_eoi() {
            let token = self
                .token()
                .chain_err(|| format!("tokens parsed so far: `{}`", ast))?;
            ast.push(token);
            self.cmt()
        }
        Ok(ast)
    }
}
