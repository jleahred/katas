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
pub(crate) fn parse_ref_rule_on_left_recursion<'a, CustI>(
    rules: &'a SetOfRules<CustI>,
    status: Status<'a>,
    rule_name: &RuleName,
) -> Result<'a> {
    println!("parsing REC ref_rule on ________ {}", status.pos.n);

    //  lock parsing expr at any point
    //  call parse expanded rule
    //  if ok
    //      insert in cache
    //      callback expanded rule not locking

    // let rule_index = *rules.get_index_by_rulename(rule_name).unwrap();
    // let (status, left_recursion) = status.push_parsing_rule(rule_index);
    // if left_recursion {
    //     panic!("detected lef recursion on {}", rule_name.0);
    // }
    let rule_index = *rules.get_index_by_rulename(rule_name).unwrap();
    let start_pos = status.pos.clone();

    let original_status = status.clone();

    let status_locked_rule = status.lock_rule(rule_index);

    let rule_info = rules.get_ri(rule_name).map(|exp| exp.clone());
    // todo: if missing rule, critic error, not follow
    let to_fill_cachestatus = match rule_info {
        Some(ri) => crate::expr::parse(rules, status_locked_rule, &ri.expr),
        None => Err(status_locked_rule.to_error(&format!("missing rule {}", rule_name.0))),
    }?;
    let original_status_with_cache =
        original_status.update_cache(rule_index, start_pos, to_fill_cachestatus);

    // todo: if missing rule, critic error, not follow

    // let status = parse_ref_rule(rules, original_status, rule_name)?;
    // let status = parse_ref_rule(rules, status, rule_name)?;
    // dbg!(&original_status_with_cache.parsed_rules_cache);
    // let original_status_with_cache = original_status_with_cache.unlock_rule(rule_index);
    // let status = parse_ref_rule(rules, original_status_with_cache, rule_name)?;
    let status = original_status_with_cache.unlock_rule(rule_index);
    let status = parse_ref_rule(rules, status, rule_name)?;
    let status = status.unlock_rule(rule_index);
    let status = parse_ref_rule(rules, status, rule_name)?;

    // let status = match rule_info {
    //     Some(ri) => crate::expr::parse(rules, original_status_with_cache, &ri.expr),
    //     None => Err(original_status_with_cache.to_error(&format!("missing rule {}", rule_name.0))),
    // }?;
    // let status = status.unlock_rule(rule_index);
    // // let status = parse_ref_rule(rules, original_status_with_cache, rule_name)?;
    // let status = parse_ref_rule(rules, status, rule_name)?;

    // dbg!(&status);
    println!(
        "parsing REC ref_rule on ========================================== {}",
        status.pos.n
    );

    let status = status.update_max_rec_needed();

    Ok(status)
}

pub(crate) fn parse_ref_rule_on_left_recursion_till_max<'a, CustI>(
    rules: &'a SetOfRules<CustI>,
    status: Status<'a>,
    rule_name: &RuleName,
    max_rec: usize,
) -> Result<'a> {
    println!("parsing REC2 ref_rule on ________ {}", status.pos.n);

    if max_rec == 0 {
        Ok(status)
    } else {
        let rule_index = *rules.get_index_by_rulename(rule_name).unwrap();
        let rule_info = rules.get_ri(rule_name).map(|exp| exp.clone());
        let status_orig = status.clone();
        let status = status.lock_rule(rule_index);
        // todo: if missing rule, critic error, not follow
        let r = match rule_info {
            Some(ri) => crate::expr::parse(rules, status, &ri.expr),
            None => Err(status.to_error(&format!("missing rule {}", rule_name.0))),
        };
        match r {
            Ok(status) => {
                parse_ref_rule_on_left_recursion_till_max(rules, status, rule_name, max_rec - 1)
            }
            Err(_) => Ok(status_orig),
        }
    }
}

pub(crate) fn parse_ref_rule<'a, CustI>(
    rules: &'a SetOfRules<CustI>,
    status: Status<'a>,
    rule_name: &RuleName,
) -> Result<'a> {
    println!("parsing ref_rule on {}", status.pos.n);

    //
    //  left recursion hack -----------------
    let rule_index = *rules.get_index_by_rulename(rule_name).unwrap();

    //  if in cache, return it
    let (status, cached_status) = status.get_status_parsed_cache(rule_index);
    if let Some(st) = cached_status {
        return Ok(st);
    }

    if status.is_rule_locked(rule_index) {
        return Err(status.to_error(&format!("failed solving left recursion")));
    }

    let (status, left_recursion) = status.push_parsing_rule(rule_index);
    if left_recursion {
        // let status = parse_ref_rule_on_left_recursion(rules, status, rule_name)?;
        // dbg!(&status.pos.n);
        // return Ok(status);
        return parse_ref_rule_on_left_recursion_till_max(rules, status, rule_name, 10);
    }
    //  left recursion hack -------
    //

    let rule_info = rules.get_ri(rule_name).map(|exp| exp.clone());
    // todo: if missing rule, critic error, not follow
    match rule_info {
        Some(ri) => crate::expr::parse(rules, status, &ri.expr),
        None => Err(status.to_error(&format!("missing rule {}", rule_name.0))),
    }

    //  in order to deal with left recursion
    //
    //  if result in parse rule cache
    //      return it Ok
    //  if locked rule  (locking rule == 1)
    //      return Error
    //  increase counter locking rule if so
    //
    //   if left_recursion
    //      init locking rule if so
    //      call recursivily
    //      if ok
    //          remove locking rule
    //          call recursivilly

    // let rule_index = *rules.get_index_by_rulename(rule_name).unwrap();
    // let start_pos = status.pos.clone();

    // //  todo
    // //  manage left recursion --------------------------

    // //  if in cache, return it
    // let (status, cached_status) = status.get_status_parsed_cache(rule_index);
    // // if let Some(st) = cached_status {
    // //     return Ok(st);
    // // }

    // let (status, left_recursion) = status.push_parsing_rule(rule_index);

    // //  if locked rule, return error
    // if left_recursion {
    //     // not in cache  //   status.is_rule_locked(rule_index) {
    //     return Err(status.to_error(&format!(
    //         "trying to break infinite left recursion {}",
    //         rule_name.0
    //     )));
    // }

    // // increase locked rule if so
    // let status = status.increase_locked_if_so(rule_index);

    // if left_recursion && !status.is_rule_locked(rule_index) {
    //     let status = status.init_locking_rule_if_so(rule_index);
    //     let status = parse_ref_rule(rules, status, rule_name)?;
    //     // if status.pos.n == 5 {
    //     //     dbg!(&status);
    //     // }
    //     let status = status
    //         .remove_lock_rule(rule_index)
    //         .remove_rule_from_parsing_rules(rule_index)
    //         //a
    //         ;
    //     let r = parse_ref_rule(rules, status, rule_name);
    //     return r;
    // }
    // // let status = match left_recursion {
    // //     true => status.init_locking_rule_if_so(rule_index),
    // //     false => status,
    // // };
    // // if left_recursion {
    // //     return parse_ref_rule(rules, status.remove_lock_rule(rule_index), rule_name);
    // //     // let status = parse_ref_rule(rules, status.add_lock_rule(rule_index), rule_name)?;
    // //     // return parse_ref_rule(rules, status.remove_lock_rule(rule_index), rule_name);
    // // }
    // //  manage left recursion --------------------------

    // // let (status, left_recursion) = status.push_parsing_rule(rule_index);
    // let rule_info = rules.get_ri(rule_name).map(|exp| exp.clone());
    // // todo: if missing rule, critic error, not follow
    // match rule_info {
    //     Some(ri) => crate::expr::parse(rules, status, &ri.expr),
    //     None => Err(status.to_error(&format!("missing rule {}", rule_name.0))),
    // }
    // let status = status.update_cache(rule_index, start_pos);
    // Ok(status)
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
