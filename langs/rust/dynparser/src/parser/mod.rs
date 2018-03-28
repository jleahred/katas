mod atom;

use {Error, Possition};
use std::str;

//pub(self)

#[derive(Debug)]
pub struct Status<'a> {
    pos: Possition,
    parsing_desc: String,
    curr_line: String,
    text2parse: &'a str,
    t2p_iterator: str::Chars<'a>,
}

impl<'a> Status<'a> {
    #[allow(dead_code)]
    fn init(txt2prs: &'a str) -> Self {
        Status {
            pos: Possition::init(),
            parsing_desc: "".to_owned(),
            curr_line: "".to_owned(),
            text2parse: txt2prs,
            t2p_iterator: txt2prs.chars(),
        }
    }
    #[allow(dead_code)]
    pub(crate) fn set_parsing_desc(mut self, desc: &str) -> Self {
        self.parsing_desc = desc.to_owned();
        self
    }
}

impl Error {
    pub(crate) fn from_status(status: &Status) -> Self {
        Error {
            pos: status.pos.clone(),
            descr: status.parsing_desc.clone(),
            on_line: status.curr_line.clone(),
        }
    }
}
