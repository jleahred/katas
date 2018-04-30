//-----------------------------------------------------------------------
//
//  mod parser::atom  TEST
//
//-----------------------------------------------------------------------
use super::Status;
use super::{parse_dot, parse_literal, parse_match, Literal, Match};

#[test]
fn test_parse_literal_ok() {
    let status_init = Status::init("aaaaaaaaaaaaaaaa");
    let (status_end, last_item_parsed) = parse_literal(status_init, &Literal("aaa")).ok().unwrap();

    assert!(status_end.pos.col == 3);
    assert!(status_end.pos.n == 3);
    assert!(status_end.pos.row == 0);
    assert!(last_item_parsed == "aaa".to_owned());
}

#[test]
fn test_parse_literal_ok2() {
    let status_init = Status::init("abcdefghij");
    let (status_end, last_item_parsed) = parse_literal(status_init, &Literal("abc")).ok().unwrap();

    assert_eq!(status_end.pos.col, 3);
    assert_eq!(status_end.pos.n, 3);
    assert_eq!(status_end.pos.row, 0);
    assert!(last_item_parsed == "abc");
}

#[test]
fn test_parse_literal_fail() {
    let status_init = Status::init("abcdefghij");
    assert!(parse_literal(status_init, &Literal("bbb")).is_err());
}

#[test]
fn test_parse_literal_fail2() {
    let status_init = Status::init("abcdefghij");
    assert!(parse_literal(status_init, &Literal("abd")).is_err());
}

#[test]
fn test_parse_literal_fail_short_text2parse() {
    let status_init = Status::init("abcd");
    assert!(parse_literal(status_init, &Literal("abcdefghij")).is_err());
}

#[test]
fn test_parse_literal_with_new_line() {
    let status_init = Status::init(
        "aa
aaaaaaaaaaaaaa",
    );
    let (status_end, last_item_parsed) = parse_literal(
        status_init,
        &Literal(
            "aa
a",
        ),
    ).ok()
        .unwrap();

    assert!(status_end.pos.col == 1);
    assert!(status_end.pos.row == 1);
    assert!(
        last_item_parsed
            == "aa
a"
    );
}

#[test]
fn test_parse_dot() {
    let status = Status::init("ab");

    let (status, last_item_parsed) = parse_dot(status).ok().unwrap();
    assert!(status.pos.col == 1);
    assert!(status.pos.n == 1);
    assert!(status.pos.row == 0);
    assert!(last_item_parsed == "a");

    let (status, last_item_parsed) = parse_dot(status).ok().unwrap();
    assert!(status.pos.col == 2);
    assert!(status.pos.n == 2);
    assert!(status.pos.row == 0);
    assert!(last_item_parsed == "b");

    assert!(parse_dot(status).is_err());
}

#[test]
fn test_parse_match_ok() {
    let status = Status::init("abc_de12345 fghi");

    let match_rules = Match::new().with_chars("54321ed_cba");
    let (status, last_item_parsed) = parse_match(status, &match_rules).ok().unwrap();
    assert_eq!(status.pos.col, 11);
    assert_eq!(status.pos.n, 11);
    assert_eq!(status.pos.row, 0);
    assert_eq!(last_item_parsed, "abc_de12345");

    let (status, _last_item_parsed) = parse_dot(status).ok().unwrap();

    let match_rules = Match::new().with_bound_chars(&vec![('f', 'g'), ('h', 'j')]);
    let (status, last_item_parsed) = parse_match(status, &match_rules).ok().unwrap();
    assert_eq!(status.pos.col, 16);
    assert_eq!(status.pos.n, 16);
    assert_eq!(status.pos.row, 0);
    assert_eq!(last_item_parsed, "fghi");

    assert!(parse_match(status, &match_rules).is_err());
}

#[test]
fn test_parse_match_err() {
    let status = Status::init("abc_de12345 fghi");

    let match_rules = Match::new().with_chars("ed_cba");
    let (status, last_item_parsed) = parse_match(status, &match_rules).ok().unwrap();
    assert_eq!(status.pos.col, 6);
    assert_eq!(status.pos.n, 6);
    assert_eq!(status.pos.row, 0);
    assert_eq!(last_item_parsed, "abc_de");

    let match_rules = Match::new().with_bound_chars(&vec![('a', 'f'), ('f', 'z')]);
    assert!(parse_match(status, &match_rules).is_err());
}
