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

//-----------------------------------------------------------------------
#[derive(Debug)]
pub struct Status<'a> {
    text2parse: &'a [u8],
    pos: Possition,
}

impl<'a> Status<'a> {
    #[allow(dead_code)]
    fn init(t2p: &'a str) -> Self {
        Status {
            text2parse: t2p.as_bytes(),
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

pub fn repeat() {
    atom::repeat();
}
