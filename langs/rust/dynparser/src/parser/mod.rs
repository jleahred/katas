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

// pub struct Parsing<'a> {
//     text2parse: &'a str,
//     possition: Possition,
// }
// #[derive(Debug)]
// pub struct CurrParsing {
//     parsing_desc: &'static str,
//     starts: Possition,
//     ends: Possition,
// }

// use {Error, Possition};
// use std::str;

// //-----------------------------------------------------------------------
// //-----------------------------------------------------------------------
// #[derive(Debug)]
// pub struct Status<'a> {
//     pos: Possition,
//     parsing_desc: String,
//     curr_line: String,
//     text2parse: &'a str,

//     t2p_iterator: str::Chars<'a>,
//     curr_start_item: usize,
// }

// //-----------------------------------------------------------------------
// impl<'a> Status<'a> {
//     #[allow(dead_code)]
//     fn init(txt2prs: &'a str) -> Self {
//         Status {
//             pos: Possition::init(),
//             parsing_desc: "".to_owned(),
//             curr_line: "".to_owned(),
//             text2parse: txt2prs,
//             t2p_iterator: txt2prs.chars(),
//             curr_start_item: 0,
//         }
//     }
//     #[allow(dead_code)]
//     pub(crate) fn set_parsing_desc(mut self, desc: &str) -> Self {
//         self.parsing_desc = desc.to_owned();
//         self.start_parsing_item()
//     }

//     #[allow(dead_code)]
//     fn start_parsing_item(mut self) -> Self {
//         self.curr_start_item = self.pos.n;
//         self
//     }

//     #[allow(dead_code)]
//     pub(crate) fn get_last_item_parsed(&self) -> &'a str {
//         &self.text2parse[self.curr_start_item..self.pos.n]
//     }
// }

// //-----------------------------------------------------------------------
// //-----------------------------------------------------------------------
// impl Error {
//     pub(crate) fn from_status(status: &Status) -> Self {
//         Error {
//             pos: status.pos.clone(),
//             descr: status.parsing_desc.clone(),
//             line: status.curr_line.clone(),
//         }
//     }
// }
