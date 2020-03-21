// -------------------------------------------------------------------------------------
//  M A C R O S

/// Create a map of rules
///
/// example
/// ```
///#[macro_use]
///extern crate dpr;
///
///fn main() {
///    let ast = rules! {
///       "main"   =>  and!{
///                        lit!("aa"),
///                        ref_rule!("rule2")
///                    },
///       "rule2"  =>  and!{
///                        lit!("b"),
///                        lit!("c")
///                    }
///    }
///    .parse("aabc");
///    assert!(ast.is_ok())
///}
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
///
/// fn main() {
///     let ast = rules!{
///        "main"   =>  lit!("aa")
///     }.parse("aa");
///
///     assert!(ast.is_ok())
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
///
/// fn main() {
///     let ast = rules!{
///        "main"   =>  error!("aa")
///     }.parse("aa");
///
///     assert!(ast.is_err())
/// }
/// ```
///
/// ```rust
/// extern crate dpr;
/// use dpr::rules_from_peg;
/// fn main() {
///     let ast = rules_from_peg(
///         r#"
///
///      main    =   '('  main   ( ')'  /  error("unbalanced parenthesis") )
///              /   'hello'
///
///          "#,
///     )
///     .unwrap()
///     .parse("((hello)");
///
///     match ast {
///         Ok(_) => panic!("It should fail"),
///         Err(dpr::Error::PaserErr(e)) => assert!(e.descr == "unbalanced parenthesis"),
///         _ => panic!("unspected errro"),
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
///
/// fn main() {
///     let ast = rules!{
///        "main"   =>  and!(dot!(), dot!())
///     }.parse("aa");
///
///     assert!(ast.is_ok())
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
///
/// fn main() {
///     let ast = rules!{
///        "main"   =>  rep!(ematch!(    chlist "cd",
///                                         from 'a', to 'b',
///                                         from 'j', to 'p'
///                     ), 0)
///     }.parse("aabcdj");
///
///     assert!(ast.is_ok())
/// }
/// ```
///
///
/// You can also pass a list of chars and a vector of char bounds as next
/// example
///
/// ```
/// #[macro_use]  extern crate dpr;
///
/// fn main() {
///     let ast = rules!{
///        "main"   =>  rep!(ematch!(    chlist "cd",
///                                      from2   vec![
///                                             ('a', 'b'),
///                                             ('j', 'p')
///                                         ]
///                     ), 0)
///     }.parse("aabcdj");
///
///     assert!(ast.is_ok())
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
///
/// fn main() {
///     let ast = rules!{
///        "main"   =>  and!(dot!(), dot!())
///     }.parse("aa");
///
///     assert!(ast.is_ok())
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
///
/// fn main() {
///     let ast = rules!{
///        "main"   =>  or!(lit!("z"), lit!("a"))
///     }.parse("a");
///
///     assert!(ast.is_ok())
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
///
/// fn main() {
///     let ast = rules!{
///        "main"   =>  and!(not!(lit!("b")), dot!())
///     }.parse("a");
///
///     assert!(ast.is_ok())
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
///
/// fn main() {
///     let ast = rules!{
///        "main"   =>  rep!(lit!("a"), 0)
///     }.parse("aaaaaaaa");
///
///     assert!(ast.is_ok())
/// }
/// ```
/// repeating from 0 to infinite
///
/// ```
/// #[macro_use]  extern crate dpr;
///
/// fn main() {
///     let ast = rules!{
///        "main"   =>  rep!(lit!("a"), 0, 3)
///     }.parse("aaa");
///
///     assert!(ast.is_ok())
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
///     let ast = rules!{
///        "main" => ref_rule!("3a"),
///        "3a"   => lit!("aaa")
///     }.parse("aaa");
///
///     assert!(ast.is_ok())
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
