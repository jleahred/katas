//  mod parser::atom

use Error;
use super::Status;
use std::result;

//#[cfg(test)]
mod test;

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//
//  T Y P E S
//
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

type Result<'a> = result::Result<ParsIterator<'a>, Error>;

pub(crate) struct Literal<'a>(&'a str);
// pub(crate) struct Symbol<'a>(&'a str);
// pub(crate) struct Match(String, Vec<(char, char)>);

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//
//  A P I
//
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

#[allow(dead_code)]
pub(crate) fn parse_literal<'a>(status: Status<'a>, literal: &Literal) -> Result<'a> {
    let mut iterator = status.it(&format!("expected literal {}", literal.0));

    //  todo: waitting for try_fold
    for ch in literal.0.chars() {
        parse_ch(&mut iterator, ch)?;
    }
    Ok(iterator)
}

//-----------------------------------------------------------------------

#[allow(dead_code)]
pub(crate) fn parse_dot<'a>(status: Status<'a>) -> Result<'a> {
    let mut it = status.it(&format!("expected any char"));
    it.next().ok_or(Error::from_status(&it))?;
    Ok(it)
}

//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//  local support

use super::Possition;
use std::str;

pub(crate) struct ParsIterator<'a> {
    pos: Possition,
    parsing_desc: String,

    curr_start_item: usize,
    text2parse_it: str::Chars<'a>,
}

impl<'a> ParsIterator<'a> {
    pub(super) fn init<'b>(pos: Possition, parsing_desc: &str, text2parse: &'a str) -> Self {
        Self {
            curr_start_item: pos.n,
            pos: pos,
            parsing_desc: parsing_desc.to_owned(),
            text2parse_it: text2parse.chars(),
        }
    }
}

impl<'a> Iterator for ParsIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        Some(self.text2parse_it.next()?)
    }
}

impl Error {
    pub(crate) fn from_status(iterator: &ParsIterator) -> Self {
        Error {
            pos: iterator.pos.clone(),
            descr: iterator.parsing_desc.clone(),
            // line: status.iterator.curr_line.clone(),
            line: "todo".to_owned(),
        }
    }
}

#[allow(dead_code)]
fn parse_ch<'a>(iterator: &mut ParsIterator, ch: char) -> result::Result<(), Error> {
    if iterator.next().ok_or(Error::from_status(iterator))? == ch {
        Ok(())
    } else {
        Err(Error::from_status(&iterator))
    }
}

impl<'a> Status<'a> {
    #[allow(dead_code)]
    pub(super) fn it(mut self, desc: &str) -> ParsIterator<'a> {
        // self.parsing_desc = desc.to_owned();
        self.iterator = ParsIterator::init(self.iterator.pos, desc, self.text2parse);
        // ParsIterator {
        //     pos: self.iterator.pos,
        //     parsing_desc: desc.to_owned(),
        //     text2parse_it: self.text2parse.chars(),
        //     curr_start_item: self.iterator.pos.n,
        // };
        self.iterator
    }
}

// impl<'a> Status<'a> {
//     #[allow(dead_code)]
//     fn next(mut self) -> result::Result<(char, Status<'a>), Error> {
//         let ch = self.iterator
//             .text2parse_it
//             .next()
//             .ok_or(Error::from_status(&self))?;
//         self.pos.n += 1;
//         match ch {
//             '\n' => {
//                 self.pos.col = 0;
//                 self.pos.row += 1;
//             }
//             '\r' => {
//                 self.pos.col = 0;
//             }
//             _ => {
//                 self.pos.col += 1;
//             }
//         }
//         Ok((ch, self))
//     }
// }
