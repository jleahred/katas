//! rules types and support

#[macro_use]
pub mod builders;

use super::expr::Expr;

/// return a set of rules
#[macro_export]
macro_rules! rules {
    ($($n:expr => $ri:expr),*) => {{
        use $crate::rules::*;

        SetOfRules::new(im::hashmap! {
            $(RuleName($n.to_string()) => $ri ,)*
        })
    }};
}

/// add additional info to an expression and returns a rule
#[macro_export]
macro_rules! rule {
    ($e:expr) => {{
        use $crate::expr::non_term::MultiExpr;
        use $crate::expr::non_term::NonTerm;
        use $crate::expr::Expr;

        RuleInfo {
            expr: $e,
            addit_inf: (),
        }
    }};
}

/// Define a rule name
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RuleName(pub String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct RuleIndex(pub usize);

/// Define a set of rules
/// `CustI` is a type defined by library client
pub struct SetOfRules<CustI> {
    rules_by_name: im::HashMap<RuleName, RuleInfo<CustI>>,
    // rule_name_by_index: im::Vector<RuleName>,
    rule_index_by_name: im::HashMap<RuleName, RuleIndex>,
}

impl<CustI> SetOfRules<CustI> {
    /// Create a set of rules with a im::HashMap<RuleName, RuleInfo<CustI>>
    pub fn new(hm: im::HashMap<RuleName, RuleInfo<CustI>>) -> Self {
        let rule_name_by_index: im::Vector<RuleName> = hm.keys().map(|rk| rk.clone()).collect();
        let rule_index_by_name = rule_name_by_index
            .iter()
            .enumerate()
            .map(|(i, rn)| (rn.clone(), RuleIndex(i)))
            .collect();
        SetOfRules {
            rules_by_name: hm,
            // rule_name_by_index,
            rule_index_by_name,
        }
    }

    pub(crate) fn get_ri(&self, rule_name: &RuleName) -> Option<&RuleInfo<CustI>> {
        self.rules_by_name.get(rule_name)
    }

    // pub(crate) fn get_rulename_by_index(&self, idx: usize) -> Option<&RuleName> {
    //     self.rule_name_by_index.get(idx)
    // }

    pub(crate) fn get_index_by_rulename(&self, rule_name: &RuleName) -> Option<&RuleIndex> {
        self.rule_index_by_name.get(rule_name)
    }
}

/// Rule information
#[derive(Debug, Clone)]
pub struct RuleInfo<CustI> {
    /// Expression on rule info
    pub expr: Expr,
    /// Additional info configured for rule (generic data)
    pub addit_inf: CustI,
}

/// Return a rule info from an expression
pub fn ri(expr: Expr) -> RuleInfo<()> {
    RuleInfo {
        expr,
        addit_inf: (),
    }
}

/// Return a rule info from an expression
pub fn rai<CustIo>(expr: Expr, addit_inf: CustIo) -> RuleInfo<CustIo> {
    RuleInfo { expr, addit_inf }
}
