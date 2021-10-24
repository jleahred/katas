#![warn(missing_docs)]
// #![feature(external_doc)]
// #![doc(include = "../README.md")]

//! For an introduction and context view, read...
//!
//! [README.md pending](https://github.com/jleahred/...)
//!
//! A very basic example...
//! ```rust
//!```
//!
//!
//!
//! Please, read [README.md pending](https://github.com/jleahred/...) for
//! more context information
//!

#[macro_use]
pub mod expr;
pub mod result;
#[macro_use]
pub mod rules;
pub(crate) mod status;

#[cfg(test)]
mod test;

use result::*;
use rules::*;
use status::Status;

// --------------------------------------------------------------------------------
/// Parse a string with for a given set of rules
pub fn parse<'a, CustI>(text: &'a str, rules: &SetOfRules<CustI>) -> Result<'a> {
    let status = Status::init(text);
    expr::non_term::parse_ref_rule(rules, status, &RuleName("main".to_string()))
        .map_err(|e| Error {
            pos: e.status.pos,
            expected: e.expected,
        })?
        .check_finalization()
}

// --------------------------------------------------------------------------------
impl<'a> Status<'a> {
    fn check_finalization(&self) -> Result<'a> {
        if self.pos.n == self.text2parse.len() {
            Ok(())
        } else {
            Err(Error::from_status(&self, "not consumed full input"))
        }
    }
}
