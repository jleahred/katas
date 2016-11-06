mod msg_parse;
#[macro_use]
mod test_diff;


#[cfg(test)]
mod tests {
    use super::msg_parse::Parsing;
    use super::test_diff;

    #[test]
    fn it_works() {
        let init = Parsing::default();
        let mut finish = init.clone();
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
}
