#![warn(missing_docs)]
//! Module with functions to generate rules from PEG grammar
//!

mod gcode;
pub mod peg2code;
mod rules;

use dynparser as dp;
use idata::{self, cont::IVec};
use std::{self, result};

#[cfg(test)]
mod test;

struct Context {
    //  stack with the module paths we are inside
//  i.e.   mod_a, mod_a.mod_b, mod_a.mod_b, mod_c
// _inside_mods__for_future: Vec<String>,
}

impl Context {
    fn new() -> Self {
        Context {
            // _inside_mods__for_future: vec![],
        }
    }
}

#[derive(Debug)]
/// Most of peg functions will return a result with this type
/// on Error side
pub enum Error {
    /// When error has been on `peg` side
    /// we will receive a description and
    /// optionally, a link to a stacked error
    /// Then, we can have a errors stack of ilimited size
    Peg((String, Option<Box<Error>>)),
    /// When error is on parser side
    Parser(dp::parser::Error),
    /// When error is on ast side
    Ast(dp::ast::Error),
}

fn error_peg_s(s: &str) -> Error {
    Error::Peg((s.to_string(), None))
}

impl Error {
    fn ipush(self, desc: &str) -> Self {
        Error::Peg((desc.to_string(), Some(Box::new(self))))
    }
}

impl From<dp::parser::Error> for Error {
    fn from(e: dp::parser::Error) -> Self {
        Error::Parser(e)
    }
}

impl From<dp::ast::Error> for Error {
    fn from(e: dp::ast::Error) -> Self {
        Error::Ast(e)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Peg((s, None)) => write!(f, "{}", s),
            Error::Peg((s, Some(b))) => write!(f, "{} > {}", s, b),
            Error::Parser(p) => write!(f, "Parser({:?})", p),
            Error::Ast(a) => write!(f, "AST({:?})", a),
        }
    }
}

/// Most of functions on peg module, will return a set of rules
/// or an error
pub type Result = result::Result<dp::parser::expression::SetOfRules, Error>;

// -------------------------------------------------------------------------------------
//  A P I

/// Given a ```peg``` set of rules on an string, it will generate
/// the set of rules to use in the parser
///
/// Next, is a full example showing the error messages, if so
/// ```
/// extern crate dynparser;
/// use dp::{parse, rules_from_peg};
///
/// fn main() {
///     let rules = rules_from_peg(
///         r#"
/// main    =   'hello'   ' '   'world'  dot
/// dot     =   "\0x2E"
///         "#,
///     ).map_err(|e| {
///         println!("{}", e);
///         panic!("FAIL");
///     })
///         .unwrap();
///
///     println!("{:#?}", rules);
///
///     let result = parse("hello world.", &rules);
///
///     assert!(result.is_ok());
///
///     match result {
///         Ok(ast) => println!("{:#?}", ast),
///         Err(e) => println!("Error: {:?}", e),
///     };
/// }
/// ```
///
/// Next is an example with some ```and``` ```literals```
/// and comments on peg grammar
/// ```
///extern crate dynparser;
///use dp::{parse, rules_from_peg};
///
///    let rules = rules_from_peg(
///        r#"
///         //  classic hello world
///         main    =   'hello'   ' '   'world'
///
///         /*  with a multiline comment
///         */
///        "#,
///    ).unwrap();
///
///     assert!(parse("hello world", &rules).is_ok());
/// ```
///
/// Next is an example with some  error info
///
/// ```
///    extern crate dynparser;
///    use dp::{parse, rules_from_peg};
///
///    let rules = rules_from_peg(
///        r#"
///         main    =   '('  main  ( ')'  /  error("unbalanced parenthesys") )
///                 /   'hello'
///        "#,
///    ).unwrap();
///
///     assert!(parse("hello", &rules).is_ok());
///     println!("{:?}", parse("(hello)", &rules));
///     assert!(parse("(hello)", &rules).is_ok());
///     assert!(parse("((hello))", &rules).is_ok());
///     assert!(parse("(((hello)))", &rules).is_ok());
///     match parse("(hello", &rules) {
///         Err(e) => {assert!(e.descr == "unbalanced parenthesys");},
///         _ => ()
///     }
///     match parse("((hello)", &rules) {
///         Err(e) => {assert!(e.descr == "unbalanced parenthesys");},
///         _ => ()
///     }
/// ```

pub fn rules_from_peg(peg: &str) -> Result {
    let ast = dp::parse(peg, &rules::parse_peg())?;
    let nodes = ast.compact().prune(&["_", "_1", "_eol"]).flatten();

    rules_from_flat_ast(&nodes)
}

//  A P I
// -------------------------------------------------------------------------------------

fn rules_from_flat_ast(nodes: &[dp::ast::flat::Node]) -> Result {
    let (rules, nodes, _context) = consume_main(&nodes, Context::new())?;
    if !nodes.is_empty() {
        Err(error_peg_s("expected empty nodes after processing main"))
    } else {
        Ok(rules)
    }
}

macro_rules! push_err {
    ($descr:expr, $e:expr) => {{
        let l = move || $e;
        l().map_err(move |e: Error| e.ipush($descr))
    }};
}

fn consuming_rule<'a, F, R>(
    rule_name: &str,
    nodes: &'a [dp::ast::flat::Node],
    context: Context,
    f: F,
) -> result::Result<(R, &'a [dp::ast::flat::Node], Context), Error>
where
    F: FnOnce(
        &'a [dp::ast::flat::Node],
        Context,
    ) -> result::Result<(R, &'a [dp::ast::flat::Node], Context), Error>, //result::Result<(dp::parser::expression::SetOfRules, &'a [dp::ast::flat::Node]), Error>
                                                                         // R: std::ops::Try,
{
    push_err!(&format!("consuming {}", rule_name), {
        let nodes = dp::ast::flat::consume_node_start_rule_name(rule_name, &nodes)?;
        let (result, nodes, context) = f(&nodes, context)?;
        let nodes = dp::ast::flat::consume_node_end_rule_name(rule_name, &nodes)?;
        Ok((result, nodes, context))
    })
}

fn consume_main(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::SetOfRules,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    // main            =   grammar

    consuming_rule("main", nodes, context, |nodes, context| {
        consume_grammar(&nodes, context)
    })
}

fn consume_grammar(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::SetOfRules,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    // grammar         =   rule+

    fn rec_consume_rules(
        rules: dp::parser::expression::SetOfRules,
        nodes: &[dp::ast::flat::Node],
        context: Context,
    ) -> result::Result<
        (
            dp::parser::expression::SetOfRules,
            &[dp::ast::flat::Node],
            Context,
        ),
        Error,
    > {
        match dp::ast::flat::peek_first_node(nodes)? {
            dp::ast::flat::Node::BeginRule(_) => {
                let ((name, expr), nodes, context) = consume_rule(nodes, context)?;
                let rules = rules.add(&name, expr);
                rec_consume_rules(rules, nodes, context)
            }
            _ => Ok((rules, nodes, context)),
        }
    }
    //  --------------------------

    consuming_rule("grammar", nodes, context, |nodes, context| {
        rec_consume_rules(dp::rules!(), &nodes, context)
    })
}

type StringExpression = (String, dp::parser::expression::Expression);
fn consume_rule(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(StringExpression, &[dp::ast::flat::Node], Context), Error> {
    // rule            =   _  rule_name  _  '='  _  expr  _eol _

    consuming_rule("rule", nodes, context, |nodes, context| {
        let (rule_name, nodes, context) = consume_rule_name(nodes, context)?;
        let nodes = dp::ast::flat::consume_this_value("=", nodes)?;
        let (expr, nodes, context) = consume_peg_expr(nodes, context)?;

        Ok(((rule_name, expr), nodes, context))
    })
}

fn consume_rule_name(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(String, &[dp::ast::flat::Node], Context), Error> {
    // rule_name       =   '.'?  symbol  ('.' symbol)*

    fn rec_consume_dot_symbol(
        acc_name: String,
        nodes: &[dp::ast::flat::Node],
        context: Context,
    ) -> result::Result<(String, &[dp::ast::flat::Node], Context), Error> {
        match dp::ast::flat::peek_first_node(nodes)? {
            dp::ast::flat::Node::Val(ch) => {
                if ch == "." {
                    let (_, nodes) = dp::ast::flat::consume_val(nodes)?;
                    let (symbol, nodes, context) = consume_symbol(nodes, context)?;
                    let acc_name = format!("{}.{}", acc_name, symbol);
                    rec_consume_dot_symbol(acc_name, nodes, context)
                } else {
                    Ok((acc_name, nodes, context))
                }
            }
            _ => Ok((acc_name, nodes, context)),
        }
    }

    let get_dot_or_empty =
        |nodes, context| -> result::Result<(&str, &[dp::ast::flat::Node], Context), Error> {
            match dp::ast::flat::peek_first_node(nodes)? {
                dp::ast::flat::Node::Val(ch) => {
                    if ch == "." {
                        Ok((".", dp::ast::flat::consume_this_value(".", nodes)?, context))
                    } else {
                        Ok(("", nodes, context))
                    }
                }
                _ => Ok(("", nodes, context)),
            }
        };
    //  ----------------------

    consuming_rule("rule_name", nodes, context, |nodes, context| {
        let (start_dot, nodes, context) = get_dot_or_empty(nodes, context)?;
        let (symbol, nodes, context) = consume_symbol(nodes, context)?;
        let (dot_symbol, nodes, context) = rec_consume_dot_symbol(String::new(), nodes, context)?;

        let str_result = format!("{}{}{}", start_dot, symbol, dot_symbol);
        Ok((str_result, nodes, context))
    })
}

fn consume_symbol(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(&str, &[dp::ast::flat::Node], Context), Error> {
    // symbol          =   [_'a-zA-Z0-9] [_'"a-zA-Z0-9]*

    consuming_rule("symbol", nodes, context, |nodes, context| {
        let (val, nodes) = dp::ast::flat::consume_val(nodes)?;
        Ok((val, nodes, context))
    })
}

fn consume_peg_expr(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    //  expr            =   or

    consuming_rule("expr", nodes, context, |nodes, context| {
        consume_or(nodes, context)
    })
}

//  This is to manage And & Or multiexpressions
//  in consume_or and consume_and
enum ExprOrVecExpr {
    Expr(dp::parser::expression::Expression),
    VExpr(Vec<dp::parser::expression::Expression>),
    None,
}
impl ExprOrVecExpr {
    fn ipush(self, expr: dp::parser::expression::Expression) -> Self {
        match self {
            ExprOrVecExpr::Expr(e) => ExprOrVecExpr::VExpr(vec![e, expr]),
            ExprOrVecExpr::VExpr(v) => ExprOrVecExpr::VExpr(v.ipush(expr)),
            ExprOrVecExpr::None => ExprOrVecExpr::Expr(expr),
        }
    }
}

fn consume_or(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    // or              =   and         ( _  '/'  _  or )?

    fn rec_consume_or(
        eov: ExprOrVecExpr,
        nodes: &[dp::ast::flat::Node],
        context: Context,
    ) -> result::Result<(ExprOrVecExpr, &[dp::ast::flat::Node], Context), Error> {
        consuming_rule("or", nodes, context, |nodes, context| {
            let (expr, nodes, context) = consume_and(nodes, context)?;
            let eov = eov.ipush(expr);
            let next_node = dp::ast::flat::peek_first_node(nodes)?;

            match next_node {
                dp::ast::flat::Node::Val(_) => {
                    let nodes = dp::ast::flat::consume_this_value("/", nodes)?;
                    rec_consume_or(eov, nodes, context)
                }
                _ => Ok((eov, nodes, context)),
            }
        })
    };

    let build_or_expr =
        |vexpr| dp::parser::expression::Expression::Or(dp::parser::expression::MultiExpr(vexpr));
    //  --------------------------

    push_err!("or:", {
        let (eov, nodes, context) = rec_consume_or(ExprOrVecExpr::None, nodes, context)?;

        match eov {
            ExprOrVecExpr::None => Err(error_peg_s("logic error, empty or parsing???")),
            ExprOrVecExpr::Expr(e) => Ok((e, nodes, context)),
            ExprOrVecExpr::VExpr(v) => Ok((build_or_expr(v), nodes, context)),
        }
    })
}

fn consume_error(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    // error           =   'error' _ '('  _  literal  _  ')'
    let (val, nodes, context) = consuming_rule("error", nodes, context, |nodes, context| {
        let nodes = dp::ast::flat::consume_this_value("error", nodes)?;
        let nodes = dp::ast::flat::consume_this_value("(", nodes)?;
        let (text, nodes, context) = consume_literal_string(nodes, context)?;
        let nodes = dp::ast::flat::consume_this_value(")", nodes)?;
        Ok((text, nodes, context))
    })?;

    Ok((dp::error!(val), nodes, context))
}

fn consume_and(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    // and             =   error
    //                 /   named? rep_or_neg  ( _1 _ !(rule_name _ ('=' / '{')) and )*

    fn consume_named(
        nodes: &[dp::ast::flat::Node],
        context: Context,
    ) -> result::Result<(Option<&str>, &[dp::ast::flat::Node], Context), Error> {
        println!("_____ {:#?}", nodes);
        consuming_rule("named", nodes, context, |nodes, context| {
            let (symbol, nodes, context) = consume_symbol(nodes, context)?;
            let (_, nodes) = dp::ast::flat::consume_val(nodes)?; //  ':'
            Ok((Some(symbol), nodes, context))
        })
    }

    fn rec_consume_and(
        eov: ExprOrVecExpr,
        nodes: &[dp::ast::flat::Node],
        context: Context,
    ) -> result::Result<(ExprOrVecExpr, &[dp::ast::flat::Node], Context), Error> {
        consuming_rule("and", nodes, context, |nodes, context| {
            if "error" == dp::ast::flat::get_nodename(dp::ast::flat::peek_first_node(nodes)?)? {
                let (expr, nodes, context) = consume_error(nodes, context)?;
                let eov = eov.ipush(expr);
                Ok((eov, nodes, context))
            } else {
                //  todo working
                // println!("_________ {:#?}", nodes);
                let (named, nodes, context) = match dp::ast::flat::peek_first_node(nodes)? {
                    dp::ast::flat::Node::BeginRule(rule_name) => {
                        if rule_name == "named" {
                            consume_named(nodes, context)
                        } else {
                            Ok((None, nodes, context))
                        }
                    }
                    _ => Ok((None, nodes, context)),
                }?;

                let (expr, nodes, context) = consume_rep_or_neg(nodes, context)?;
                let eov = eov.ipush(expr);
                let next_node = dp::ast::flat::peek_first_node(nodes)?;

                match (next_node, dp::ast::flat::get_nodename(next_node)) {
                    (dp::ast::flat::Node::BeginRule(_), Ok("and")) => {
                        rec_consume_and(eov, nodes, context)
                    }
                    _ => Ok((eov, nodes, context)),
                }
            }
        })
    }

    let build_and_expr =
        |vexpr| dp::parser::expression::Expression::And(dp::parser::expression::MultiExpr(vexpr));
    //  --------------------------

    let (eov, nodes, context) = rec_consume_and(ExprOrVecExpr::None, nodes, context)?;
    match eov {
        ExprOrVecExpr::None => Err(error_peg_s("logic error, empty or parsing???")),
        ExprOrVecExpr::Expr(e) => Ok((e, nodes, context)),
        ExprOrVecExpr::VExpr(v) => Ok((build_and_expr(v), nodes, context)),
    }
}

fn consume_rep_or_neg(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    // rep_or_neg      =   atom_or_par ("*" / "+" / "?")?
    //                 /   "!" atom_or_par

    fn process_repetition_indicator(
        expr: dp::parser::expression::Expression,
        rsymbol: &str,
    ) -> result::Result<dp::parser::expression::Expression, Error> {
        match rsymbol {
            "+" => Ok(dp::rep!(expr, 1)),
            "*" => Ok(dp::rep!(expr, 0)),
            "?" => Ok(dp::rep!(expr, 0, 1)),
            unknown => Err(error_peg_s(&format!(
                "repetition symbol unknown {}",
                unknown
            ))),
        }
    }

    let atom_and_rep = |nodes, context| {
        let (expr, nodes, context) = consume_atom_or_par(nodes, context)?;
        let next_node = dp::ast::flat::peek_first_node(nodes)?;

        match next_node {
            dp::ast::flat::Node::Val(_) => {
                let (sep, nodes) = dp::ast::flat::consume_val(nodes)?;
                Ok((process_repetition_indicator(expr, sep)?, nodes, context))
            }
            _ => Ok((expr, nodes, context)),
        }
    };
    let neg_and_atom = |nodes,
                        context|
     -> result::Result<
        (
            dp::parser::expression::Expression,
            &[dp::ast::flat::Node],
            Context,
        ),
        Error,
    > {
        let nodes = dp::ast::flat::consume_this_value(r#"!"#, nodes)?;
        let (expr, nodes, context) = consume_atom_or_par(nodes, context)?;
        Ok((dp::not!(expr), nodes, context))
    };
    //  --------------------------

    consuming_rule("rep_or_neg", nodes, context, |nodes, context| {
        match dp::ast::flat::peek_first_node(nodes)? {
            dp::ast::flat::Node::Val(v) => {
                if v == "!" {
                    neg_and_atom(nodes, context)
                } else {
                    Err(error_peg_s(&format!("expected '!', received {}", v)))
                }
            }
            _ => atom_and_rep(nodes, context),
        }
    })
}

fn consume_atom_or_par(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    // atom_or_par     =   (atom / parenth)

    consuming_rule("atom_or_par", nodes, context, |nodes, context| {
        let next_node = dp::ast::flat::peek_first_node(nodes)?;
        let node_name = dp::ast::flat::get_nodename(next_node)?;

        let (expr, nodes, context) = push_err!(&format!("n:{}", node_name), {
            match &node_name as &str {
                "atom" => consume_atom(nodes, context),
                "parenth" => consume_parenth(nodes, context),
                unknown => Err(error_peg_s(&format!("unknown {}", unknown))),
            }
        })?;

        Ok((expr, nodes, context))
    })
}

fn consume_atom(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    // atom            =   literal
    //                 /   match
    //                 /   dot
    //                 /   rule_name

    consuming_rule("atom", nodes, context, |nodes, context| {
        let next_node = dp::ast::flat::peek_first_node(nodes)?;
        let node_name = dp::ast::flat::get_nodename(next_node)?;

        let (expr, nodes, context) = push_err!(&format!("n:{}", node_name), {
            match &node_name as &str {
                "literal" => consume_literal_expr(nodes, context),
                "rule_name" => consume_rule_ref(nodes, context),
                "dot" => consume_dot(nodes, context),
                "match" => consume_match(nodes, context),
                unknown => Err(error_peg_s(&format!("unknown {}", unknown))),
            }
        })?;

        Ok((expr, nodes, context))
    })
}

fn consume_parenth(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    //  parenth         =   "("  _  expr  _  ")"

    consuming_rule("parenth", nodes, context, |nodes, context| {
        let nodes = dp::ast::flat::consume_this_value(r#"("#, nodes)?;
        let (expr, nodes, context) = consume_peg_expr(nodes, context)?;
        let nodes = dp::ast::flat::consume_this_value(r#")"#, nodes)?;
        Ok((expr, nodes, context))
    })
}

fn consume_literal_string(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(String, &[dp::ast::flat::Node], Context), Error> {
    // literal         =  lit_noesc  /  lit_esc

    consuming_rule("literal", nodes, context, |nodes, context| {
        let next_node_name = dp::ast::flat::get_nodename(dp::ast::flat::peek_first_node(nodes)?)?;
        match next_node_name {
            "lit_noesc" => consume_literal_no_esc(nodes, context),
            "lit_esc" => consume_literal_esc(nodes, context),
            _ => Err(error_peg_s(&format!("unexpected node {}", next_node_name))),
        }
    })
}

fn consume_literal_expr(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    let (val, nodes, context) = consume_literal_string(nodes, context)?;
    Ok((dp::lit!(val), nodes, context))
}

fn consume_literal_esc(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(String, &[dp::ast::flat::Node], Context), Error> {
    // lit_esc         =   _"
    //                         (   esc_char
    //                         /   hex_char
    //                         /   !_" .
    //                         )*

    fn consume_element(
        nodes: &[dp::ast::flat::Node],
        context: Context,
    ) -> result::Result<(String, &[dp::ast::flat::Node], Context), Error> {
        let crule_name = |rule_name, nodes, context| match rule_name {
            "esc_char" => consume_esc_char(nodes, context),
            "hex_char" => consume_hex_char(nodes, context),
            _ => Err(error_peg_s(&format!("unknown rule_name: {}", rule_name))),
        };

        let next_n = dp::ast::flat::peek_first_node(nodes)?;

        let (val, nodes, context) = match next_n {
            dp::ast::flat::Node::BeginRule(r_name) => crule_name(r_name, nodes, context),
            dp::ast::flat::Node::Val(_) => {
                let (val, nodes) =
                    dp::ast::flat::consume_val(nodes).map(|(v, n)| (v.to_string(), n))?;
                Ok((val, nodes, context))
            }
            _ => Err(error_peg_s(&format!("looking for element {:#?}", next_n))),
        }?;

        Ok((val.to_string(), nodes, context))
    }

    fn rec_consume_lit_esc_ch(
        s: String,
        nodes: &[dp::ast::flat::Node],
        context: Context,
    ) -> result::Result<(String, &[dp::ast::flat::Node], Context), Error> {
        let next_node_name = dp::ast::flat::get_nodename(dp::ast::flat::peek_first_node(nodes)?);

        match next_node_name {
            Ok("_\"") => Ok((s, nodes, context)),
            _ => {
                let (v, nodes, context) = consume_element(nodes, context)?;
                rec_consume_lit_esc_ch(s + &v.to_string(), nodes, context)
            }
        }
    }

    consuming_rule("lit_esc", nodes, context, |nodes, context| {
        let (nodes, context) = consume_quote(nodes, context)?;

        let (val, nodes, context) = rec_consume_lit_esc_ch(String::new(), nodes, context)?;

        let vclone = val.clone();
        push_err!(&format!("lesc:({})", vclone), {
            let (nodes, context) = consume_quote(nodes, context)?;
            Ok((val, nodes, context))
        })
    })
}

fn consume_esc_char(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(String, &[dp::ast::flat::Node], Context), Error> {
    // esc_char        =   '\r'
    //                 /   '\n'
    //                 /   '\t'
    //                 /   '\\'
    //                 /   '\"'

    consuming_rule("esc_char", nodes, context, |nodes, context| {
        let (val, nodes) = dp::ast::flat::consume_val(nodes)?;
        let val = match val {
            r#"\r"# => Ok("\r"),
            r#"\n"# => Ok("\n"),
            r#"\t"# => Ok("\t"),
            r#"\\"# => Ok(r#"\"#),
            r#"\""# => Ok(r#"""#),
            _ => Err(error_peg_s(&format!("unknow esc char: {}", val))),
        }?;
        Ok((val.to_string(), nodes, context))
    })
}

fn consume_hex_char(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(String, &[dp::ast::flat::Node], Context), Error> {
    // hex_char        =   '\0x' [0-9A-F] [0-9A-F]

    use std::u8;

    consuming_rule("hex_char", nodes, context, |nodes, context| {
        let (val, nodes) = dp::ast::flat::consume_val(nodes)?;
        let val = &val[3..];

        let ch = match u8::from_str_radix(val, 16) {
            Ok(v) => Ok(v as char),
            _ => Err(error_peg_s(&format!("error parsing hex {}", &val[2..]))),
        }?;
        Ok((ch.to_string(), nodes, context))
    })
}

fn consume_literal_no_esc(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(String, &[dp::ast::flat::Node], Context), Error> {
    // lit_noesc       =   _'   (  !_' .  )*   _'
    // _'              =   "'"

    consuming_rule("lit_noesc", nodes, context, |nodes, context| {
        let (nodes, context) = consume_single_quote(nodes, context)?;
        let (val, nodes) = dp::ast::flat::consume_val(nodes)?;

        let val = val.replace(r#"\"#, r#"\\"#);
        let vclone = val.clone();
        push_err!(&format!("l:({})", vclone), {
            let (nodes, context) = consume_single_quote(nodes, context)?;
            Ok((val, nodes, context))
        })
    })
}

fn consume_quote(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(&[dp::ast::flat::Node], Context), Error> {
    // _"              =   "\u{34}"

    let (_, nodes, context) = consuming_rule(r#"_""#, nodes, context, |nodes, context| {
        Ok((
            (),
            dp::ast::flat::consume_this_value(r#"""#, nodes)?,
            context,
        ))
    })?;
    Ok((nodes, context))
}

fn consume_single_quote(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(&[dp::ast::flat::Node], Context), Error> {
    // _'              =   "'"

    let (_, nodes, context) = consuming_rule(r#"_'"#, nodes, context, |nodes, context| {
        Ok((
            (),
            dp::ast::flat::consume_this_value(r#"'"#, nodes)?,
            context,
        ))
    })?;
    Ok((nodes, context))
}

fn consume_dot(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    //  dot             =   "."

    consuming_rule("dot", nodes, context, |nodes, context| {
        let (_, nodes) = dp::ast::flat::consume_val(nodes)?;
        Ok((dp::dot!(), nodes, context))
    })
}

fn consume_rule_ref(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    push_err!("consuming symbol rule_ref", {
        let (symbol_name, nodes, context) = consume_rule_name(nodes, context)?;

        Ok((dp::ref_rule!(symbol_name), nodes, context))
    })
}

fn consume_match(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<
    (
        dp::parser::expression::Expression,
        &[dp::ast::flat::Node],
        Context,
    ),
    Error,
> {
    // match           =   "["
    //                         (
    //                             (mchars  mbetween*)
    //                             / mbetween+
    //                         )
    //                     "]"

    type VecChCh = Vec<(char, char)>;
    consuming_rule("match", nodes, context, |nodes, context| {
        fn rec_consume_mbetween(
            acc: Vec<(char, char)>,
            nodes: &[dp::ast::flat::Node],
            context: Context,
        ) -> result::Result<(VecChCh, &[dp::ast::flat::Node], Context), Error> {
            let next_node = dp::ast::flat::peek_first_node(nodes)?;
            let node_name = dp::ast::flat::get_nodename(next_node);
            match node_name {
                Ok("mbetween") => {
                    let ((from, to), nodes, context) = consume_mbetween(nodes, context)?;
                    rec_consume_mbetween(acc.ipush((from, to)), nodes, context)
                }
                _ => Ok((acc, nodes, context)),
            }
        }
        //  --------------------------

        let nodes = dp::ast::flat::consume_this_value("[", nodes)?;

        let (omchars, nodes, context) =
            match dp::ast::flat::get_nodename(dp::ast::flat::peek_first_node(nodes)?)? {
                "mchars" => {
                    let (mchars, nodes, context) = consume_mchars(nodes, context)?;
                    (Some(mchars), nodes, context)
                }
                _ => (None, nodes, context),
            };

        let (vchars, nodes, context) = rec_consume_mbetween(vec![], nodes, context)?;

        let (expr, nodes) = match (omchars, vchars.is_empty()) {
            (Some(chars), true) => Ok((dp::ematch!(chlist chars, from2 vec![]), nodes)),
            (Some(chars), false) => Ok((dp::ematch!(chlist chars, from2 vchars), nodes)),
            (None, false) => Ok((dp::ematch!(chlist "", from2 vchars), nodes)),
            _ => Err(error_peg_s("Invalid match combination")),
        }?;

        let nodes = dp::ast::flat::consume_this_value("]", nodes)?;

        Ok((expr, nodes, context))
    })
}

fn consume_mchars(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(&str, &[dp::ast::flat::Node], Context), Error> {
    // mchars          =   (!"]" !(. "-") .)+

    consuming_rule("mchars", nodes, context, |nodes, context| {
        let (val, nodes) = dp::ast::flat::consume_val(nodes)?;
        Ok((val, nodes, context))
    })
}

type CharChar = (char, char);
fn consume_mbetween(
    nodes: &[dp::ast::flat::Node],
    context: Context,
) -> result::Result<(CharChar, &[dp::ast::flat::Node], Context), Error> {
    // mbetween        =   (.  "-"  .)

    consuming_rule("mbetween", nodes, context, |nodes, context| {
        let (from_to, nodes) = dp::ast::flat::consume_val(nodes)?;

        let (from, chars) = idata::consume_char(from_to.chars())
            .ok_or_else(|| error_peg_s("expected from char"))?;
        let (_, chars) =
            idata::consume_char(chars).ok_or_else(|| error_peg_s("expected '-' char"))?;
        let (to, _) = idata::consume_char(chars).ok_or_else(|| error_peg_s("expected to char"))?;
        Ok(((from, to), nodes, context))
    })
}
