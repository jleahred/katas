use crate::rules::RuleIndex;

use super::result::Possition;
use std::str::Chars;

#[derive(Debug, Clone)]
pub(crate) struct Status<'a> {
    pub(crate) text2parse: &'a str,
    pub(crate) it_parsing: Chars<'a>,
    pub(crate) pos: Possition,
    pub(crate) deeper_error: Option<Box<Error<'a>>>,

    // left recursion hack -----------------
    /// set of rules been parsed at this point
    /// this let us to detect left recursion
    left_recursion: StatusLeftRecursion,
    // left recursion hack -----------------
}

#[derive(Debug, Clone)]
pub(crate) struct StatusLeftRecursion {
    pub(crate) pos_and_rules_on_parsing_and_depth: im::HashMap<PosAndRulesOnParsing, u32>,
}

#[derive(Debug, Clone)]
pub(crate) struct Error<'a> {
    pub(crate) status: Status<'a>,
    pub(crate) expected: im::Vector<String>,
}

pub(crate) type Result<'a> = std::result::Result<Status<'a>, Error<'a>>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct PosAndRulesOnParsing {
    npos: usize,
    rule_index: RuleIndex,
}

pub(crate) struct StopLeftRecursion(pub(crate) bool);

impl<'a> Status<'a> {
    pub(crate) fn init(text2parse: &'a str) -> Self {
        Status {
            text2parse,
            it_parsing: text2parse.chars(),
            pos: Possition::init(),
            deeper_error: None,
            left_recursion: StatusLeftRecursion {
                pos_and_rules_on_parsing_and_depth: im::hashmap! {},
            },
        }
    }

    /// returns true if left recursion detected
    pub(crate) fn lr_push_parsing_rule(
        mut self,
        rule_index: RuleIndex,
    ) -> (Self, StopLeftRecursion) {
        let parsing_rule = pos_and_parsing_rule_from_status(&self, rule_index);

        let left_recursion_found = self
            .left_recursion
            .pos_and_rules_on_parsing_and_depth
            .get(&parsing_rule);
        let left_recurson_depth = match left_recursion_found {
            None => 0,
            Some(lrd) => lrd + 1,
        };

        self.left_recursion
            .pos_and_rules_on_parsing_and_depth
            .insert(parsing_rule, left_recurson_depth);

        let stop_left_recursion = match left_recurson_depth {
            0 | 1 => StopLeftRecursion(false),
            _ => StopLeftRecursion(true),
        };

        (self, stop_left_recursion)
    }

    pub(crate) fn to_error(self, expected: &str) -> Error<'a> {
        Error {
            status: self,
            expected: im::vector![expected.to_owned()],
        }
    }

    pub(crate) fn merge_deeper_error(mut self, err: &Option<Error<'a>>) -> Self {
        let error = match (&self.deeper_error, err) {
            (None, e) => e.clone(),
            (Some(e1), Some(e2)) => Some(merge_errors(e1, e2)),
            (Some(e1), None) => Some(e1.as_ref().clone()),
        };
        self.deeper_error = error.and_then(|e| Some(Box::new(e)));
        self
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

pub(crate) fn merge_errors<'a>(err1: &Error<'a>, err2: &Error<'a>) -> Error<'a> {
    match err1.status.pos.n.cmp(&err2.status.pos.n) {
        std::cmp::Ordering::Equal => Error {
            status: err2.status.clone(),
            expected: err1.expected.clone() + err2.expected.clone(),
        },
        std::cmp::Ordering::Less => err2.clone(),
        std::cmp::Ordering::Greater => err1.clone(),
    }
}
