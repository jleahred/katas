//! Support for NON terminal symbols

use super::Expr;
use crate::rules::{RuleName, SetOfRules};
use crate::status::{Error, Result, Status};

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

pub(crate) fn get_left_recursion_max_count<'a, CustI>(
    rules: &'a SetOfRules<CustI>,
    status: Status<'a>,
    rule_name: &RuleName,
    rule_info: &'a crate::rules::RuleInfo<CustI>,
    rec_count: usize,
) -> std::result::Result<(Status<'a>, usize), Error<'a>> {
    println!("parsing REC2 ref_rule on ________> {}", status.pos.n);

    let rule_index = *rules.get_index_by_rulename(rule_name).unwrap();
    let status_orig = status.clone();
    let status = status.lock_rule(rule_index);
    // todo: if missing rule, critic error, not follow
    let r = crate::expr::parse(rules, status, &rule_info.expr);
    match r {
        Ok(status) => {
            get_left_recursion_max_count(rules, status, rule_name, rule_info, rec_count + 1)
        }
        Err(_) => Ok((status_orig, rec_count)),
    }
}

pub(crate) fn parse_ref_rule<'a, CustI>(
    rules: &'a SetOfRules<CustI>,
    status: Status<'a>,
    rule_name: &RuleName,
) -> Result<'a> {
    println!("parsing ref_rule on {}", status.pos.n);

    let rule_info = rules
        .get_ri(rule_name)
        .map(|exp| exp.clone())
        .ok_or_else(|| {
            status
                .clone()
                .to_error(&format!("missing rule {}", rule_name.0))
        })?;
    // let rule_info = match orule_info {
    //     Some(ri) => crate::expr::parse(rules, status, &ri.expr),
    //     None => Err(status.to_error(&format!("missing rule {}", rule_name.0))),
    // }

    //
    //  left recursion hack -----------------
    let rule_index = *rules.get_index_by_rulename(rule_name).unwrap();

    //  if in cache, return it
    // let (status, cached_status) = status.get_status_parsed_cache(rule_index);
    // if let Some(st) = cached_status {
    //     dbg!(&st);
    //     return Ok(st);
    // }

    if status.is_rule_locked(rule_index) {
        return Err(status.to_error(&format!("failed solving left recursion")));
    }

    let (status, left_recursion) = status.push_parsing_rule(rule_index);
    if left_recursion {
        let (status, rec_found) =
            get_left_recursion_max_count(rules, status, rule_name, rule_info, 0)?;
        dbg!(rec_found);
        return Ok(status);
    }
    //  left recursion hack -------
    //

    crate::expr::parse(rules, status, &rule_info.expr)
    // todo: if missing rule, critic error, not follow
    // match rule_info {
    //     Some(ri) => crate::expr::parse(rules, status, &ri.expr),
    //     None => Err(status.to_error(&format!("missing rule {}", rule_name.0))),
    // }
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
    let mut first_error = None;
    for expr in &multi_expr.0 {
        match super::parse(rules, status.clone(), &expr) {
            Ok(s) => return Ok(s),
            Err(e) => first_error = Some(e),
        }
    }
    Err(first_error.unwrap())
}
