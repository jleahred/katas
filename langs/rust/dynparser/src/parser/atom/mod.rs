pub enum TailCall<T, R> {
    Call(T),
    Return(R),
}

pub fn tail_call<T, R, E, F>(seed: T, recursive_function: F) -> result::Result<R, E>
where
    F: Fn(T) -> result::Result<TailCall<T, R>, E>,
{
    let mut state = TailCall::Call(seed);
    loop {
        match state {
            TailCall::Call(arg) => {
                state = recursive_function(arg)?;
            }
            TailCall::Return(result) => {
                return Ok(result);
            }
        }
    }
}

// fn factorial(x: u64) -> u64 {
//     tail_call((x, 1), |(x, acc)| {
//         if x <= 1 {
//             TailCall::Return(acc)
//         } else {
//             TailCall::Call((x - 1, acc * x))
//         }
//     })
// }

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//
//
//  mod parser::atom
//
//
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

use std::result;
use super::{Error, Status};

//-----------------------------------------------------------------------
//
//  T Y P E S
//
//-----------------------------------------------------------------------
pub(crate) struct Literal<'a>(&'a str);
type Result<'a> = result::Result<Status<'a>, Error>;

#[cfg(test)]
mod test;

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//
//  A P I
//
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

#[allow(dead_code)]
pub(crate) fn parse_literal<'a>(status: Status<'a>, literal: &Literal) -> Result<'a> {
    let status = status.parsing_descr(format!("parsing literal {}", literal.0));

    let init_tc = (literal.0[0..].as_bytes(), Ok(status));

    tail_call(init_tc, |(pend_lit, acc)| {
        if pend_lit.len() == 0 {
            Ok(TailCall::Return(acc?))
        } else {
            let (b, st) = acc?.get_byte()?;
            match b == pend_lit[0] {
                true => Ok(TailCall::Call((&pend_lit[1..], Ok(st)))),
                false => Err(Error::from_status(&st)),
            }
        }
    })

    // let mut new_status = status.set_parsing_desc(&format!("expected literal {}", literal.0));

    // for ch in literal.0.chars() {
    //     new_status = parse_ch(new_status, ch)?;
    // }
    // Ok(new_status)
}

impl<'a> Status<'a> {
    #[allow(dead_code)]
    fn get_byte(mut self) -> result::Result<(u8, Status<'a>), Error> {
        if self.text2parse.len() == 0 {
            Err(Error::from_status(&self))
        } else {
            self.pos.n += 1;
            let byte = self.text2parse[0];
            match byte as char {
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
            Ok((byte, self))
        }
    }
}
// use Error;
// use super::Status;
// use std::result;

// use std::str;

// #[cfg(test)]
// mod test;

// //-----------------------------------------------------------------------
// //-----------------------------------------------------------------------
// //
// //  T Y P E S
// //
// //-----------------------------------------------------------------------
// //-----------------------------------------------------------------------

// type Result<'a> = result::Result<Status<'a>, Error>;

// pub(crate) struct Literal<'a>(&'a str);
// pub(crate) struct Symbol<'a>(&'a str);
// // pub(crate) struct Match(String, Vec<(char, char)>);

// //-----------------------------------------------------------------------
// //-----------------------------------------------------------------------
// //
// //  A P I
// //
// //-----------------------------------------------------------------------
// //-----------------------------------------------------------------------

// #[allow(dead_code)]
// pub(crate) fn parse_literal<'a>(status: Status<'a>, literal: &Literal) -> Result<'a> {
//     let mut new_status = status.set_parsing_desc(&format!("expected literal {}", literal.0));

//     for ch in literal.0.chars() {
//         new_status = parse_ch(new_status, ch)?;
//     }
//     Ok(new_status)
// }

// //-----------------------------------------------------------------------

// #[allow(dead_code)]
// pub(crate) fn parse_dot<'a>(status: Status<'a>) -> Result<'a> {
//     let (_, result_status) = status.set_parsing_desc("expected any char").get_char()?;
//     Ok(result_status)
// }

// //-----------------------------------------------------------------------

// #[allow(dead_code)]
// pub(crate) fn parse_symbol<'a>(status: Status<'a>) -> Result<'a> {
//     fn is_char_symbol(ch: char) -> bool {
//         match ch {
//             'a'...'z' => true,
//             _ => false,
//         }
//     }

//     let nwst0 = status.set_parsing_desc("expected symbol");
//     let error = Error::from_status(&nwst0);

//     match nwst0.peek_ch() {
//         Some((_, nwst1)) => Ok(nwst1),
//         None => Err(error),
//     }
//     // let Some((ch, nwst1)) = nwst0.peek_ch();

//     // // while let Some((ch, mut nwst)) = nwst.peek_ch() {
//     // //     if is_char_symbol(ch) {
//     // //         nwst.get_char2();
//     // //     }
//     // // }
//     // Ok(nwst1)
// }

// //-----------------------------------------------------------------------

// //-----------------------------------------------------------------------
// //-----------------------------------------------------------------------
// //  local support

// #[allow(dead_code)]
// fn parse_ch(status: Status, ch: char) -> Result {
//     let (got_ch, new_status) = status.get_char()?;
//     if got_ch == ch {
//         Ok(new_status)
//     } else {
//         Err(Error::from_status(&new_status))
//     }
// }

// impl<'a> Status<'a> {
//     #[allow(dead_code)]
//     fn get_byte(mut self) -> result::Result<(u8, Status<'a>), Error> {
//         let ch = self.t2p_iterator.next().ok_or(Error::from_status(&self))?;
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

//     #[allow(dead_code)]
//     fn get_char2(&mut self) -> Option<char> {
//         let ch = self.t2p_iterator.next()?;
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
//         Some(ch)
//     }

//     fn peek_ch(self) -> Option<(char, Self)> {
//         let ch = self.t2p_iterator.peekable().peek()?.clone();
//         Some((ch, self))
//     }
// }

// // use std::iter;
// // fn peek<I>(it: &iter::Peekable<I>) -> Option<char>
// // where
// //     I: Iterator<Item = char>,
// // {
// //     // Some(self.t2p_iterator.peekable().peek()?.clone())
// //     Some(*it.peekable().peek()?)
// //     // None
// // }

// // fn trawl<E, I>(it: Peekable<I>) where I: Iterator<Result<char, E>> {
// //     ...
// // }
