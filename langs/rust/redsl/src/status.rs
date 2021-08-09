use crate::rules::RuleIndex;

use super::result::Possition;
use std::str::Chars;

#[derive(Debug, Clone)]
pub(crate) struct Status<'a> {
    pub(crate) text2parse: &'a str,
    pub(crate) it_parsing: Chars<'a>,
    pub(crate) pos: Possition,

    // left recursion hack -----------------
    /// set of rules been parsed at this point
    /// this let us to detect left recursion
    pub(crate) pos_and_rules_on_parsing: im::HashSet<PosAndRulesOnParsing>,
    pub(crate) locking_rules: im::HashSet<RuleIndex>,
    pub(crate) parsed_rules_cache: im::HashMap<PosAndRulesOnParsing, Status<'a>>,
    pub(crate) max_lef_recursion_needed: usize,
    // left recursion hack -----------------
    // //  to manage left recursion (look into parse_ref_rule)
    // //  if 0, in process of locking, if 1, rule is already locked
    // pub(crate) locking_rules: im::HashMap<RuleIndex, usize>,
}

// pub(crate) type Error = super::result::Error;
#[derive(Debug)]
pub(crate) struct Error<'a> {
    pub(crate) status: Status<'a>,
    pub(crate) description: String,
}

pub(crate) type Result<'a> = std::result::Result<Status<'a>, Error<'a>>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct PosAndRulesOnParsing {
    npos: usize,
    rule_index: RuleIndex,
}

impl<'a> Status<'a> {
    pub(crate) fn init(text2parse: &'a str) -> Self {
        Status {
            text2parse,
            it_parsing: text2parse.chars(),
            pos: Possition::init(),
            pos_and_rules_on_parsing: im::hashset! {},
            parsed_rules_cache: im::hashmap! {},
            locking_rules: im::hashset![],
            max_lef_recursion_needed: 0,
        }
    }

    pub(crate) fn update_max_rec_needed(mut self) -> Self {
        self.max_lef_recursion_needed = std::cmp::max(self.max_lef_recursion_needed, self.pos.n);
        self
    }
    /// returns true if left recursion detected
    pub(crate) fn push_parsing_rule(mut self, rule_index: RuleIndex) -> (Self, bool) {
        let parsing_rule = pos_and_parsing_rule_from_status(&self, rule_index);

        let left_recursion = self.pos_and_rules_on_parsing.contains(&parsing_rule);

        self.pos_and_rules_on_parsing.insert(parsing_rule);
        (self, left_recursion)
    }

    // pub(crate) fn remove_rule_from_parsing_rules(mut self, rule_index: RuleIndex) -> Self {
    //     let parsing_rule = pos_and_parsing_rule_from_status(&self, rule_index);
    //     self.pos_and_rules_on_parsing.remove(&parsing_rule);
    //     self
    // }

    pub(crate) fn update_cache(
        mut self,
        rule_index: RuleIndex,
        pos: Possition,
        from_status: Status<'a>,
    ) -> Self {
        let prp = PosAndRulesOnParsing {
            npos: pos.n,
            rule_index,
        };
        self.parsed_rules_cache.insert(prp, from_status.clone());
        self
    }

    pub(crate) fn merge_cache(mut self, from_status: Status<'a>) -> Self {
        for (k, v) in &from_status.parsed_rules_cache {
            self.parsed_rules_cache.insert(k.clone(), v.clone());
        }
        self
    }

    //  it returns status and cached status
    pub(crate) fn get_status_parsed_cache(self, rule_index: RuleIndex) -> (Self, Option<Self>) {
        let r = self
            .parsed_rules_cache
            .get(&pos_and_parsing_rule_from_status(&self, rule_index))
            .map(|s| s.clone());
        (self, r)
    }

    pub(crate) fn lock_rule(mut self, rule_index: RuleIndex) -> Self {
        self.locking_rules.insert(rule_index);
        self
    }

    pub(crate) fn unlock_rule(mut self, rule_index: RuleIndex) -> Self {
        self.locking_rules.remove(&rule_index);
        self
    }

    // pub(crate) fn init_locking_rule_if_so(mut self, rule_index: RuleIndex) -> Self {
    //     self.locking_rules = self.locking_rules.update_with(rule_index, 0, |o, _n| o);
    //     self
    // }

    // pub(crate) fn increase_locked_if_so(mut self, rule_index: RuleIndex) -> Self {
    //     self.locking_rules = match self.locking_rules.get(&rule_index) {
    //         Some(_v) => {
    //             self.locking_rules
    //                 .update_with(rule_index, 1, |o, _n| if o == 0 { 1 } else { 1 })
    //         }
    //         None => self.locking_rules,
    //     };
    //     self
    // }

    pub(crate) fn is_rule_locked(&self, rule_index: RuleIndex) -> bool {
        self.locking_rules.contains(&rule_index)
    }

    // pub(crate) fn remove_lock_rule(mut self, rule_index: RuleIndex) -> Self {
    //     self.locking_rules.remove(&rule_index);
    //     // self.locking_rules.update(rule_index, 0);
    //     self
    // }

    pub(crate) fn to_error(self, description: &str) -> Error<'a> {
        Error {
            status: self,
            description: description.to_owned(),
        }
    }
}

fn pos_and_parsing_rule_from_status(
    status: &Status,
    rule_index: RuleIndex,
) -> PosAndRulesOnParsing {
    PosAndRulesOnParsing {
        npos: status.pos.n,
        rule_index,
    }
}
