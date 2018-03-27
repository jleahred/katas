use Error;
// use Error;
use super::Status;

#[allow(dead_code)]
pub(super) fn parse_ch(status: Status, text2parse: &str, ch: char) -> Result<Status, Error> {
    get_ch(status, &text2parse, ch)
}
#[test]
fn test_parse_ch() {
    let status_init = Status::init();
    let status_end = Status::init();
    assert!(parse_ch(status_init, "aaaabbbb", 'a').ok().unwrap() == status_end);
}

#[allow(dead_code)]
pub(super) fn get_ch(status: Status, text2parse: &str, ch: char) -> Result<(char, &str), Error> {
    let error_eof = |st: &Status| {
        Err(Error {
            pos: st.pos.clone(),
            descr: format!("expected {} on EOF", ch),
            line_text: st.curr_line.clone(),
        })
    };

    match text2parse.chars().next() {
        None => error_eof(&status),
        Some(ch) => Ok((ch, &text2parse[2..])),
    }
}

impl Status {
    fn next(mut self, ch: char) -> Self {
        self.pos.n += 1;
        match ch {
            '\n' => {
                self.pos.col = 0;
                self.pos.row += 1;
            }
            '\r' => {
                self.pos.col = 0;
            }
            _ => {
                self.pos.col += 1;
            }
        }
        self
    }
}
// #[allow(dead_code)]
// pub(super) fn parse_literal(status: Status, _text2parse: &str, lit: &str) -> Result<Status, Error> {
//     Ok(status)
//     // Err(Error {
//     //     pos: Possition::init(),
//     //     descr: "description".to_owned(),
//     //     line_text: "line".to_owned(),
//     // })
// }

// #[test]
// fn test_parse_literal() {
//     let status_init = Status::init();
//     let status_end = Status::init();
//     assert!(parse_literal(status_init, "aaaabbbb", "aaa").ok().unwrap() == status_end);
// }
