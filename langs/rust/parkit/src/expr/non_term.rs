//! Support for NON terminal symbols

use super::Expr;
use crate::rules::{RuleName, SetOfRules};
use crate::status::{Result, Status};

/// Opaque type to manage multiple expressions
#[derive(Debug, PartialEq, Clone)]
pub struct MultiExpr(pub im::vector::Vector<Expr>);

/// Define NON terminal symbols
#[derive(Debug, PartialEq, Clone)]
pub enum NonTerm {
    /// Concatenate expressions
    And(MultiExpr),
    /// Try till get a matching rule
    Or(MultiExpr),
    /// Reference to a rule name
    RefRule(RuleName),
}

pub(crate) fn parse<'a, CustI>(
    rules: &'a SetOfRules<CustI>,
    status: Status<'a>,
    non_term: &'a NonTerm,
) -> Result<'a> {
    match &non_term {
        &NonTerm::And(me) => parse_and(rules, status, &me),
        &NonTerm::Or(me) => parse_or(rules, status, &me),
        &NonTerm::RefRule(rn) => parse_ref_rule(rules, status, &rn),
    }
}

//-----------------------------------------------------------------------

pub(crate) fn parse_ref_rule<'a, CustI>(
    rules: &'a SetOfRules<CustI>,
    status: Status<'a>,
    rule_name: &RuleName,
) -> Result<'a> {
    let rule_info = rules
        .get_ri(rule_name)
        .map(|exp| exp.clone())
        .ok_or_else(|| panic!("missing rule {}", rule_name.0))?;

    //  if in cache, return it
    // let (status, cached_status) = status.get_status_parsed_cache(rule_index);
    // if let Some(st) = cached_status {
    //     dbg!(&st);
    //     return Ok(st);
    // }

    //
    //  left recursion hack --------------------------------------------------------------------
    let rule_index = *rules.get_index_by_rulename(rule_name).unwrap();
    let (status, stop_left_recursion) = status.lr_push_parsing_rule(rule_index);
    if stop_left_recursion.0 {
        //panic!("left recursion detected parsing {}", rule_name.0)
        return Err(status.to_error("left recursion"));
        //return Ok(status);
    }
    //  left recursion hack --------------------------------------------------------------------
    //

    crate::expr::parse(rules, status, &rule_info.expr)
}

//-----------------------------------------------------------------------
fn parse_and<'a, CustI>(
    rules: &'a SetOfRules<CustI>,
    status: Status<'a>,
    multi_expr: &'a MultiExpr,
) -> Result<'a> {
    let mut status = status.clone();
    for expr in &multi_expr.0 {
        status = super::parse(rules, status, &expr)?;
    }
    Ok(status)
}

//-----------------------------------------------------------------------
fn parse_or<'a, CustI>(
    rules: &'a SetOfRules<CustI>,
    status: Status<'a>,
    multi_expr: &'a MultiExpr,
) -> Result<'a> {
    let mut error = None;
    for expr in &multi_expr.0 {
        match (super::parse(rules, status.clone(), &expr), &mut error) {
            (Ok(s), _) => return Ok(s),
            (Err(e), None) => error = Some(e),
            (Err(e), Some(prev_error)) => error = Some(crate::status::merge(prev_error, &e)),
        }
    }
    Err(error.unwrap())
}
