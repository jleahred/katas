use msg_parse::{ParsingInfo, errors};
use test_diff;


#[test]
fn init_add_char() {
    let mut parsing = ParsingInfo::new();
    parsing.add_char('1');

    let check = ParsingInfo {
        orig_msg: "1".to_string(),
        msg_length: 1,
        reading_tag: "1".to_string(),
        ..Default::default()
    };

    assert_eq_dif!(parsing, check);
}


#[test]
fn init_add_chars() {
    let mut parsing = ParsingInfo::new();
    parsing.add_char('1');
    parsing.add_char('2');
    parsing.add_char('3');
    parsing.add_char('4');
    parsing.add_char('5');

    let check = ParsingInfo {
        orig_msg: "12345".to_string(),
        msg_length: 5,
        reading_tag: "12345".to_string(),
        ..Default::default()
    };

    assert_eq_dif!(parsing, check);
}

#[test]
fn invalid_first_char() {
    let mut parsing = ParsingInfo::new();
    parsing.add_char('a');

    let check = ParsingInfo {
        orig_msg: "a".to_string(),
        msg_length: 1,
        reading_tag: "a".to_string(),
        current_field_error: Some((1, errors::TAG_INVALID_CHAR)),
        ..Default::default()
    };

    assert_eq_dif!(parsing, check);
}

#[test]
fn invalid_second_char() {
    let mut parsing = ParsingInfo::new();
    parsing.add_char('1');
    parsing.add_char('a');

    let check = ParsingInfo {
        orig_msg: "1a".to_string(),
        msg_length: 2,
        reading_tag: "1a".to_string(),
        current_field_error: Some((2, errors::TAG_INVALID_CHAR)),
        ..Default::default()
    };

    assert_eq_dif!(parsing, check);
}


#[test]
fn invalid_chars_2errors() {
    let mut parsing = ParsingInfo::new();
    parsing.add_char('1');
    parsing.add_char('a');
    parsing.add_char('b');

    let check = ParsingInfo {
        orig_msg: "1ab".to_string(),
        msg_length: 3,
        reading_tag: "1ab".to_string(),
        current_field_error: Some((2, errors::TAG_INVALID_CHAR)),
        ..Default::default()
    };

    assert_eq_dif!(parsing, check);
}

#[test]
fn invalid_chars_2errors_andvalids() {
    let mut parsing = ParsingInfo::new();
    parsing.add_char('1');
    parsing.add_char('2');
    parsing.add_char('a');
    parsing.add_char('b');
    parsing.add_char('3');
    parsing.add_char('4');

    let check = ParsingInfo {
        orig_msg: "12ab34".to_string(),
        msg_length: 6,
        reading_tag: "12ab34".to_string(),
        current_field_error: Some((3, errors::TAG_INVALID_CHAR)),
        ..Default::default()
    };

    assert_eq_dif!(parsing, check);
}


#[test]
fn invalid_chars_2errors_and_valids_non_consecutives() {
    let mut parsing = ParsingInfo::new();
    parsing.add_char('1');
    parsing.add_char('2');
    parsing.add_char('a');
    parsing.add_char('3');
    parsing.add_char('b');
    parsing.add_char('4');
    parsing.add_char('5');

    let check = ParsingInfo {
        orig_msg: "12a3b45".to_string(),
        msg_length: 7,
        reading_tag: "12a3b45".to_string(),
        current_field_error: Some((3, errors::TAG_INVALID_CHAR)),
        ..Default::default()
    };

    assert_eq_dif!(parsing, check);
}
