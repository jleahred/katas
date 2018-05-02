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
mod expression;

use {Error, Possition};
use std::str::Chars;
use std::result;

//-----------------------------------------------------------------------
#[derive(Debug)]
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

type Result<'a> = result::Result<(Status<'a>, String), Error>;
