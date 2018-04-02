use super::Status;
use super::{parse_dot, parse_literal, Literal};

//#[test]
fn test_parse_dot() {
    let status_init = Status::init("ab");
    let status1 = parse_dot(status_init).ok().unwrap();

    assert!(status1.pos.col == 1);
    assert!(status1.pos.n == 1);
    assert!(status1.pos.row == 0);
    // assert!(status1.get_last_item_parsed() == "a");

    // let status2 = parse_dot(status1).ok().unwrap();
    let status2 = parse_dot(status_init).ok().unwrap();

    assert!(status2.pos.col == 2);
    assert!(status2.pos.n == 2);
    assert!(status2.pos.row == 0);
    // assert!(status2.get_last_item_parsed() == "b");

    assert!(parse_dot(status_init).is_err());
}

//#[test]
fn test_parse_literal_ok() {
    let status_init = Status::init("aaaaaaaaaaaaaaaa");
    let status_end = parse_literal(status_init, &Literal("aaa")).ok().unwrap();

    assert!(status_end.pos.col == 3);
    assert!(status_end.pos.n == 3);
    assert!(status_end.pos.row == 0);
    // assert!(status_end.get_last_item_parsed() == "aaa");
}

//#[test]
fn test_parse_literal_ok2() {
    let status_init = Status::init("abcdefghij");
    let status_end = parse_literal(status_init, &Literal("abc")).ok().unwrap();

    assert_eq!(status_end.pos.col, 3);
    assert_eq!(status_end.pos.n, 3);
    assert_eq!(status_end.pos.row, 0);
    // assert!(status_end.get_last_item_parsed() == "abc");
}

//#[test]
fn test_parse_literal_fail() {
    let status_init = Status::init("abcdefghij");
    assert!(parse_literal(status_init, &Literal("bbb")).is_err());
}

//#[test]
fn test_parse_literal_fail2() {
    let status_init = Status::init("abcdefghij");
    assert!(parse_literal(status_init, &Literal("abd")).is_err());
}

//#[test]
fn test_parse_literal_with_new_line() {
    let status_init = Status::init(
        "aa
aaaaaaaaaaaaaa",
    );
    let status_end = parse_literal(
        status_init,
        &Literal(
            "aa
a",
        ),
    ).ok()
        .unwrap();

    assert!(status_end.pos.col == 1);
    assert!(status_end.pos.row == 1);
    // assert!(
    //         status_end.get_last_item_parsed()
    //             == "aa
    // a"
    // );
}
