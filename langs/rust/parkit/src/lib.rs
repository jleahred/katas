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

fn from_status_error_2_result_error(e: status::Error) -> Error {
    Error {
        pos: e.status.pos.clone(),
        expected: e.expected,
        line: e
            .status
            .text2parse
            .lines()
            .into_iter()
            .skip(e.status.pos.row)
            .take(1)
            .collect(),
    }
}

// --------------------------------------------------------------------------------
/// Parse a string with for a given set of rules
pub fn parse<'a, CustI>(text: &'a str, rules: &SetOfRules<CustI>) -> Result<'a> {
    let status = Status::init(text);
    expr::non_term::parse_ref_rule(rules, status, &RuleName("main".to_string()))
        .map_err(from_status_error_2_result_error)?
        .check_finalization()
}

// --------------------------------------------------------------------------------
//  todo:
impl<'a> Status<'a> {
    fn check_finalization(&self) -> Result<'a> {
        if self.pos.n == self.text2parse.len() {
            Ok(())
        } else {
            match &self.deeper_error {
                None => Err(Error {
                    pos: self.pos.clone(),
                    expected: im::vector!["not consumed full input...".to_owned()],
                    line: self
                        .text2parse
                        .lines()
                        .into_iter()
                        .skip(self.pos.row)
                        .take(1)
                        .collect(),
                }),
                Some(e) => Err(from_status_error_2_result_error(*e.clone())),
            }
        }
    }
}
