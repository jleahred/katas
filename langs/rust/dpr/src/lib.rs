#![warn(missing_docs)]
// #![feature(external_doc)]
// #![doc(include = "../README.md")]

//! For an introduction and context view, read...
//!
//! [README.md](https://github.com/jleahred/dpr)
//!
//! A very basic example...
//! ```rust
//!```
//!
//!
//!
//! Please, read [README.md](https://github.com/jleahred/dpr) for
//! more context information
//!

// -------------------------------------------------------------------------------------
//  M A C R O S

/// Create a map of rules
///
/// example
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  and!{
///                         lit!("aa"),
///                         ref_rule!("rule2")
///                     },
///        "rule2"  =>  and!{
///                         lit!("b"),
///                         lit!("c")
///                     }
///     };
///
///     assert!(parse("aabc", &rules).is_ok())
/// }
/// ```
#[macro_export]
#[cfg_attr(feature = "cargo-clippy", allow(clippy::let_and_return))]
macro_rules! rules {
    ($($n:expr => $e:expr),*) => {{
        use $crate::parser::expression;
        use std::collections::HashMap;

        let rules = expression::SetOfRules::new(HashMap::<String, expression::Expression>::new());
        $(let rules = rules.add($n, $e);)*
        rules
    }};
}

/// Create a literal
///
/// example
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  lit!("aa")
///     };
///
///     assert!(parse("aa", &rules).is_ok())
/// }
/// ```
#[macro_export]
macro_rules! lit {
    ($e:expr) => {{
        $crate::parser::expression::Expression::Simple($crate::parser::atom::Atom::Literal(
            $e.to_string(),
        ))
    }};
}

/// Generate an error
///
/// example
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  error!("aa")
///     };
///
///     assert!(parse("aa", &rules).is_err())
/// }
/// ```
///
/// ```rust
/// extern crate dpr;
/// use dpr::{parse, rules_from_peg};
/// fn main() {
///     let rules = rules_from_peg(
///         r#"
///
///     main    =   '('  main   ( ')'  /  error("unbalanced parenthesis") )
///             /   'hello'
///
///         "#,
///     ).unwrap();
///
///     match parse("((hello)", &rules) {
///         Ok(_) => panic!("It should fail"),
///         Err(e) => assert!(e.descr == "unbalanced parenthesis"),
///     }
/// }
/// ```

#[macro_export]
macro_rules! error {
    ($e:expr) => {{
        $crate::parser::expression::Expression::Simple($crate::parser::atom::Atom::Error(
            $e.to_string(),
        ))
    }};
}

/// Atom::Dot (any character)
///
/// example
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  and!(dot!(), dot!())
///     };
///
///     assert!(parse("aa", &rules).is_ok())
/// }
/// ```
#[macro_export]
macro_rules! dot {
    () => {{
        $crate::parser::expression::Expression::Simple($crate::parser::atom::Atom::Dot)
    }};
}

/// Generate a match expression with optional characters and a list
/// of bounds
///
///  "String", from 'a', to 'b', from 'c', to 'd'
/// The first string, is a set of chars.
/// Later you can write a list of tuples with ranges to validate
///
/// example
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  rep!(ematch!(    chlist "cd",
///                                         from 'a', to 'b',
///                                         from 'j', to 'p'
///                     ), 0)
///     };
///
///     assert!(parse("aabcdj", &rules).is_ok())
/// }
/// ```
///
///
/// You can also pass a list of chars and a vector of char bounds as next
/// example
///
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  rep!(ematch!(    chlist "cd",
///                                      from2   vec![
///                                             ('a', 'b'),
///                                             ('j', 'p')
///                                         ]
///                     ), 0)
///     };
///
///     assert!(parse("aabcdj", &rules).is_ok())
/// }
/// ```

#[macro_export]
macro_rules! ematch {
    (chlist $chars:expr, $(from $from:expr,  to $to:expr),*) => {{
        //use idata::cont::IVec;  //  pending macros by example 2.0
        use $crate::parser;
        let mut v = Vec::<(char, char)>::new();

        //$(let v = v.ipush(($from, $to));)+  //  pending macros by example 2.0
        $(v.push(($from, $to));)+
        let amatch = parser::atom::Atom::Match(parser::atom::MatchRules::init($chars, v));
        parser::expression::Expression::Simple(amatch)
    }};

    (chlist $chars:expr, from2 $vfrom2:expr) => {{
        use $crate::parser;

        let amatch = parser::atom::Atom::Match(parser::atom::MatchRules::init($chars, $vfrom2));
        parser::expression::Expression::Simple(amatch)
    }};
}

/// Concat expressions (and)
///
/// example
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  and!(dot!(), dot!())
///     };
///
///     assert!(parse("aa", &rules).is_ok())
/// }
/// ```
#[macro_export]
macro_rules! and {
    ($($e:expr),*) => {{
        use $crate::parser::expression::{Expression, MultiExpr};

        Expression::And(MultiExpr::new(vec![$($e ,)*]))
    }};
}

/// Choose expressions (or)
///
/// example
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  or!(lit!("z"), lit!("a"))
///     };
///
///     assert!(parse("a", &rules).is_ok())
/// }
/// ```
#[macro_export]
macro_rules! or {
    ($($e:expr),*) => {{
        use $crate::parser::expression::{Expression, MultiExpr};

        Expression::Or(MultiExpr::new(vec![$($e ,)*]))
    }};
}

/// negate expression
///
/// example
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  and!(not!(lit!("b")), dot!())
///     };
///
///     assert!(parse("a", &rules).is_ok())
/// }
/// ```
///
/// not! will not move the parsing position
#[macro_export]
macro_rules! not {
    ($e:expr) => {{
        $crate::parser::expression::Expression::Not(Box::new($e))
    }};
}

/// repeat expression.
/// You have to define minimum repetitions and optionally
/// maximum repetitions (if missing, infinite)
///
/// example
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  rep!(lit!("a"), 0)
///     };
///
///     assert!(parse("aaaaaaaa", &rules).is_ok())
/// }
/// ```
/// repeating from 0 to infinite
///
/// ```
/// #[macro_use]  extern crate dpr;
/// use dpr::parse;
///
/// fn main() {
///     let rules = rules!{
///        "main"   =>  rep!(lit!("a"), 0, 3)
///     };
///
///     assert!(parse("aaa", &rules).is_ok())
/// }
/// ```
#[macro_export]
macro_rules! rep {
    ($e:expr, $min:expr) => {{
        use $crate::parser::expression;

        expression::Expression::Repeat(expression::RepInfo::new(Box::new($e), $min, None))
    }};

    ($e:expr, $min:expr, $max:expr) => {{
        use $crate::parser::expression;

        expression::Expression::Repeat(expression::RepInfo::new(Box::new($e), $min, Some($max)))
    }};
}

/// This will create a subexpression referring to a "rule name"
///
/// ```
/// #[macro_use]  extern crate dpr;
///
/// fn main() {
///     let rules = rules!{
///        "main" => ref_rule!("3a"),
///        "3a"   => lit!("aaa")
///     };
///
///     assert!(dpr::parse("aaa", &rules).is_ok())
/// }
/// ```
#[macro_export]
macro_rules! ref_rule {
    ($e:expr) => {{
        $crate::parser::expression::Expression::RuleName($e.to_owned())
    }};
}

//  M A C R O S
// -------------------------------------------------------------------------------------

extern crate idata;

use std::result;

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
