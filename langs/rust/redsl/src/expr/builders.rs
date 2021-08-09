//! Functions to generate expressions
use crate::rules::RuleName;

use super::*;
use terminal::*;

/// Returns a final::Literal expression
///
/// Single literal exact
/// ```
///     extern crate redsl;
///     use redsl::expr::builders::*;
///     use redsl::*;
///
///     let rules = rules! {"main" => ri(lit("hello")) };
///     
///     parse("hello", &rules).ok().unwrap();
/// ```
///
/// Single literal diferent
/// ```
///     extern crate redsl;
///     use redsl::expr::builders::*;
///     use redsl::*;
///
///     let rules = rules! {"main" => ri(lit("_hello")) };
///     
///     parse("hello", &rules).err().unwrap();
/// ```
///
pub fn lit(literal: &str) -> Expr {
    Expr::Terminal(Terminal::Literal(literal.to_string()))
}

/// Returns a end of file expression
///
/// End of file on empty input
/// ```
///     extern crate redsl;
///     use redsl::expr::builders::*;
///     use redsl::*;
///
///     let rules = rules! {"main" => ri(eof()) };
///
///     parse("", &rules).ok().unwrap();
/// ```
///
/// End of file on non empty input
/// ```
///     use redsl::expr::builders::eof;
///     use redsl::rules;
///     
///     let rules = rules! {"main" => ri(eof()) };
///     
///     redsl::parse("aaa", &rules).err().unwrap();
/// ```
pub fn eof() -> Expr {
    Expr::Terminal(Terminal::Eof)
}

/// Returns a dot (any char) expression
///
/// ```
///     use redsl::expr::builders::dot;
///     use redsl::rules;
///     
///     let rules = rules! {"main" => ri(dot()) };
///     
///     redsl::parse("a", &rules).ok().unwrap();
/// ```
pub fn dot() -> Expr {
    Expr::Terminal(Terminal::Dot)
}

/// Returns a dot (any char) expression
///
/// ```
//      println!("Hello, world!");
//
//      let rules = rules! {
//             "main" => ror!(
//                  and!(lit("a"), ref_rule("main")),
//                  lit("a"))
//      };
//
//      let r = parse("aaaaa", &rules).ok().unwrap();
/// ```
pub fn ref_rule(rname: &str) -> Expr {
    Expr::NonTerm(NonTerm::RefRule(RuleName(rname.to_owned())))
}

/// Generate an expression and
#[macro_export]
macro_rules! and {
    ($($e:expr),*) => {{
        use $crate::expr::Expr;
        use $crate::expr::non_term::NonTerm;
        use $crate::expr::non_term::MultiExpr;

        Expr::NonTerm(NonTerm::And(MultiExpr(im::vector![$($e ,)*])))
    }};
}

/// Generate an expression or
#[macro_export]
macro_rules! or {
    ($($e:expr),*) => {{
        use $crate::expr::Expr;
        use $crate::expr::non_term::NonTerm;
        use $crate::expr::non_term::MultiExpr;

        Expr::NonTerm(NonTerm::Or(MultiExpr(im::vector![$($e ,)*])))
    }};
}
