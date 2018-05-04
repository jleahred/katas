//-----------------------------------------------------------------------
//
//  mod parser::expression  TEST
//
//-----------------------------------------------------------------------

use super::{parse, Expression, MultiExpr, Status};
use super::atom::Atom;

#[test]
fn test_parse_literal_ok() {
    let status_init = Status::init("aaaaaaaaaaaaaaaa");
    let expr = Expression::Simple(Atom::Literal("aaa"));
    let result = parse(status_init, &expr).ok().unwrap();

    assert!(result.0.pos.col == 3);
    assert!(result.0.pos.n == 3);
    assert!(result.0.pos.row == 0);
}

#[test]
fn test_parse_and_ok() {
    let status_init = Status::init("aaaaaaaaaaaaaaaa");
    let and_rules = vec![
        Expression::Simple(Atom::Literal("aa")),
        Expression::Simple(Atom::Literal("aa")),
    ];
    let expr = Expression::And(MultiExpr(&and_rules));

    let result = parse(status_init, &expr).ok().unwrap();

    assert_eq!(result.0.pos.col, 4);
    assert_eq!(result.0.pos.n, 4);
    assert_eq!(result.0.pos.row, 0);
}

#[test]
fn test_parse_and_fail() {
    let status_init = Status::init("aaaaaaaaaaaaaaaa");
    let and_rules = vec![
        Expression::Simple(Atom::Literal("aa")),
        Expression::Simple(Atom::Literal("bb")),
    ];
    let expr = Expression::And(MultiExpr(&and_rules));

    assert!(parse(status_init, &expr).is_err());
}

#[test]
fn test_parse_not_ok() {
    let status_init = Status::init("aa");

    let expr_not = Expression::Not(Box::new(Expression::Simple(Atom::Literal("bb"))));
    let result = parse(status_init, &expr_not).ok().unwrap();

    assert_eq!(result.0.pos.col, 0);
    assert_eq!(result.0.pos.n, 0);
    assert_eq!(result.0.pos.row, 0);
}

#[test]
fn test_parse_not_fail() {
    let status_init = Status::init("aa");

    let expr_not = Expression::Not(Box::new(Expression::Simple(Atom::Literal("aa"))));
    assert!(parse(status_init, &expr_not).is_err());
}

#[test]
fn test_parse_or_ok() {
    {
        let status_init = Status::init("aaaaaaaaaaaaaaaa");
        let rules = vec![
            Expression::Simple(Atom::Literal("aa")),
            Expression::Simple(Atom::Literal("aa")),
        ];
        let expr = Expression::Or(MultiExpr(&rules));

        let result = parse(status_init, &expr).ok().unwrap();

        assert_eq!(result.0.pos.col, 2);
        assert_eq!(result.0.pos.n, 2);
        assert_eq!(result.0.pos.row, 0);
    }
    {
        let status_init = Status::init("aaaaaaaaaaaaaaaa");
        let rules = vec![
            Expression::Simple(Atom::Literal("aa")),
            Expression::Simple(Atom::Literal("bb")),
        ];
        let expr = Expression::Or(MultiExpr(&rules));

        let result = parse(status_init, &expr).ok().unwrap();

        assert_eq!(result.0.pos.col, 2);
        assert_eq!(result.0.pos.n, 2);
        assert_eq!(result.0.pos.row, 0);
    }
    {
        let status_init = Status::init("aaaaaaaaaaaaaaaa");
        let rules = vec![
            Expression::Simple(Atom::Literal("bb")),
            Expression::Simple(Atom::Literal("aa")),
        ];
        let expr = Expression::Or(MultiExpr(&rules));

        let result = parse(status_init, &expr).ok().unwrap();

        assert_eq!(result.0.pos.col, 2);
        assert_eq!(result.0.pos.n, 2);
        assert_eq!(result.0.pos.row, 0);
    }
}

#[test]
fn test_parse_or_fail() {
    let status_init = Status::init("aaaaaaaaaaaaaaaa");
    let and_rules = vec![
        Expression::Simple(Atom::Literal("cc")),
        Expression::Simple(Atom::Literal("bb")),
    ];
    let expr = Expression::And(MultiExpr(&and_rules));

    assert!(parse(status_init, &expr).is_err());
}
