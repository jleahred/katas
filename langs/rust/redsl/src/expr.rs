//! Expression support
//! for terminal and not terminal symbols

#[macro_use]
pub mod builders;
pub mod non_term;
pub(crate) mod terminal;

use super::status::Status;
use crate::rules::SetOfRules;
use non_term::NonTerm;
use terminal::Terminal;

/// Expression sum type options
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    /// Terminal symbol
    Terminal(Terminal),
    /// Non terminal symbol (and, or, repetitions, reference to a rule...)
    NonTerm(NonTerm),
}

pub(crate) fn parse<'a, CustI>(
    rules: &'a SetOfRules<CustI>,
    status: Status<'a>,
    expr: &'a Expr,
) -> super::status::Result<'a> {
    match expr {
        Expr::Terminal(t) => terminal::parse(status, t),
        Expr::NonTerm(nt) => non_term::parse(rules, status, nt),
    }
}
