//! Macros and functions to generate RuleInfo from expressions

/// Generate an RuleInfo from "and"
#[macro_export]
macro_rules! rand {
    ($($e:expr),*) => {{
        use $crate::expr::Expr;
        use $crate::expr::non_term::NonTerm;
        use $crate::expr::non_term::MultiExpr;

        RuleInfo {
            expr: Expr::NonTerm(NonTerm::And(MultiExpr(im::vector![$($e ,)*]))),
            addit_inf: ()
        }
    }};
}

/// Generate an RuleInfo from "or"
#[macro_export]
macro_rules! ror {
    ($($e:expr),*) => {{
        use $crate::expr::Expr;
        use $crate::expr::non_term::NonTerm;
        use $crate::expr::non_term::MultiExpr;

        RuleInfo {
            expr: Expr::NonTerm(NonTerm::Or(MultiExpr(im::vector![$($e ,)*]))),
            addit_inf: ()
        }
    }};
}
