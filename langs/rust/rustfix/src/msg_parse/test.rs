use msg_parse::Parsing;
use test_diff;


#[test]
fn run_tests() {
    let mut finish = Parsing::default();
    finish.add_char('\u{1}');

    let check = Parsing {
        orig_msg: "^".to_string(),
        msg_length: 1,
        curr_state_length: 1,
        curr_string: "\u{1}".to_string(),
        ..Parsing::default()
    };

    assert_eq_dif!(finish, check);
}
