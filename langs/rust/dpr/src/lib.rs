#![warn(missing_docs)]
// #![feature(external_doc)]
// #![doc(include = "../README.md")]

//! For an introduction and context view, read...
//!
//! [README.md](https://github.com/jleahred/dpr)
//!
//! A very basic example...
//! ```rust
//! extern crate dpr;
//!
//! fn main() -> Result<(), dpr::Error> {
//!     let result = dpr::Peg::new(
//!         "
//!         main    =   char+
//!         char    =   'a'     -> A
//!                 /   'b'     -> B
//!                 /   .
//!     ",
//!     )
//!     .gen_rules()?
//!     .parse("aaacbbabdef")?
//!     .replace()?
//!     //  ...
//!     ;
//!
//!     assert_eq!(result, "AAAcBBABdef");
//!     Ok(())
//! }
//!```
//!
//!
//!
//! Please, read [README.md](https://github.com/jleahred/dpr) for
//! more context information
//!

extern crate idata;
extern crate im;

use std::result;

#[macro_use]
mod macros;
pub mod ast;
pub mod parser;
pub mod peg;

// -------------------------------------------------------------------------------------
//  T Y P E S

//  T Y P E S
// -------------------------------------------------------------------------------------

// -------------------------------------------------------------------------------------
//  A P I

/// Peg type for fluent API
pub struct Peg<'a>(&'a str);

/// Errors for fluent API
#[derive(Debug)]
pub enum Error {
    /// error on generating rules
    RulesErr(crate::peg::Error),
    /// error on parsing
    PaserErr(crate::parser::Error),
    /// error on replace
    ReplaceErr(String),
}

impl<'a> Peg<'a> {
    /// create an instance of Peg
    pub fn new(txt: &'a str) -> Self {
        Self(txt)
    }
    /// generate rules from peg grammar (fluent API)
    pub fn gen_rules(&self) -> result::Result<crate::parser::expression::SetOfRules, Error> {
        crate::peg::rules_from_peg(&self.0).map_err(|e| Error::RulesErr(e))
    }
}

impl crate::parser::expression::SetOfRules {
    /// parse from a set of rules (fluent API)
    pub fn parse(&self, text: &str) -> Result<ast::Node, Error> {
        crate::parse(text, self).map_err(|e| Error::PaserErr(e))
    }

    /// parse with debug info
    pub fn parse_debug(&self, text: &str) -> Result<ast::Node, Error> {
        crate::parse_debug(text, self).map_err(|e| Error::PaserErr(e))
    }
}

impl ast::Node {
    /// run the tree replacing acording the rules
    pub fn replace(&self) -> Result<crate::ast::replace::Replaced, Error> {
        ast::replace::replace(&self).map_err(|e| Error::ReplaceErr(e))
    }
}

//  A P I
// -------------------------------------------------------------------------------------

// -------------------------------------------------------------------------------------
//  A P I

fn parse(s: &str, rules: &parser::expression::SetOfRules) -> Result<ast::Node, parser::Error> {
    parse_with_debug(s, rules, false)
}

fn parse_debug(
    s: &str,
    rules: &parser::expression::SetOfRules,
) -> Result<ast::Node, parser::Error> {
    parse_with_debug(s, rules, true)
}

fn parse_with_debug(
    s: &str,
    rules: &parser::expression::SetOfRules,
    debug: bool,
) -> Result<ast::Node, parser::Error> {
    let (st, ast) = if debug {
        parser::expression::parse(parser::Status::init_debug(s, &rules, debug))?
    } else {
        parser::expression::parse(parser::Status::init(s, &rules))?
    };
    match (st.pos.n == s.len(), st.potential_error.clone()) {
        (true, _) => Ok(ast),
        (false, Some(e)) => Err(e),
        (false, None) => Err(parser::Error::from_status_normal(
            &st,
            "not consumed full input",
        )),
    }
}

pub use peg::rules_from_peg;

//  A P I
// -------------------------------------------------------------------------------------
