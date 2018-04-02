//  mod  parser

mod atom;

use Possition;
use std::str;
use self::atom::ParsIterator;

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

pub struct Status<'a> {
    // pub(super) pos: Possition,
    // pub(super) parsing_desc: String,
    // pub(super) curr_line: String,
    pub(super) text2parse: &'a str,

    iterator: ParsIterator<'a>,
    // pub(super) t2p_iterator: str::Chars<'a>,
    // pub(super) curr_start_item: usize,
}

impl<'a> Status<'a> {
    #[allow(dead_code)]
    pub(super) fn init(txt2prs: &'a str) -> Self {
        Status {
            // pos: Possition::init(),
            // parsing_desc: "".to_owned(),
            // curr_line: "".to_owned(),
            text2parse: txt2prs,
            iterator: ParsIterator::init(Possition::init(), "", txt2prs),
        }
    }
    // #[allow(dead_code)]
    // pub(super) fn it(mut self, desc: &str) -> ParsIterator<'a> {
    //     // self.parsing_desc = desc.to_owned();
    //     self.iterator = ParsIterator::init(self.iterator.pos, desc, self.text2parse);
    //     // ParsIterator {
    //     //     pos: self.iterator.pos,
    //     //     parsing_desc: desc.to_owned(),
    //     //     text2parse_it: self.text2parse.chars(),
    //     //     curr_start_item: self.iterator.pos.n,
    //     // };
    //     self.iterator
    // }

    // #[allow(dead_code)]
    // fn start_parsing_item(self) -> Self {
    //     self.iterator.curr_start_item = self.pos.n;
    //     self
    // }

    // #[allow(dead_code)]
    // pub(super) fn get_last_item_parsed(&self) -> &'a str {
    //     &self.text2parse[self.iterator.curr_start_item..self.pos.n]
    // }
}

//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
// impl Error {
//     pub(crate) fn from_status(status: &Status) -> Self {
//         Error {
//             pos: status.iterator.pos.clone(),
//             descr: status.iterator.parsing_desc.clone(),
//             // line: status.iterator.curr_line.clone(),
//             line: "todo".to_owned(),
//         }
//     }
// }
