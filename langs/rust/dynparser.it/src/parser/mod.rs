//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//
//
//  mod parser
//
//
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

mod atom;

use {Error, Possition};
use std::str::Chars;

//-----------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct Status<'a> {
    it_parsing: Chars<'a>,
    pos: Possition,
}

impl<'a> Status<'a> {
    #[allow(dead_code)]
    fn init(t2p: &'a str) -> Self {
        Status {
            it_parsing: t2p.chars(),
            pos: Possition::init(),
        }
    }

    fn it(&self) -> StIterator<'a> {
        StIterator(self.clone())
    }
}

impl Error {
    pub(crate) fn from_status(status: &Status, descr: &str) -> Self {
        Error {
            pos: status.pos.clone(),
            descr: descr.to_owned(),
            line: "pending".to_owned(),
        }
    }
}

struct StIterator<'a>(Status<'a>);

impl<'a> StIterator<'a> {
    fn status(self) -> Status<'a> {
        self.0
    }
}

impl<'a> Iterator for StIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.0.it_parsing.next()
    }
}
