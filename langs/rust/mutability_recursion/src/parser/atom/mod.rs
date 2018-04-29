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
type Result<'a> = result::Result<(Status<'a>, String), Error>;

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
pub(crate) fn parse_literal<'a>(mut status: Status<'a>, literal: &'a Literal<'a>) -> Result<'a> {
    for byte in literal.0.bytes() {
        status = parse_byte(status, byte)
            .map_err(|st| Error::from_status(&st, &format!("parsing literal {}", literal.0)))?;
    }
    Ok((status, literal.0.to_string()))
}

// #[allow(dead_code)]
// pub(crate) fn parse_literal<'a>(status: Status<'a>, literal: &'a Literal<'a>) -> Result<'a> {
//     fn parse_au8<'a>(status: Status<'a>, au8: &[u8], literal: &'a Literal<'a>) -> Result<'a> {
//         if au8.len() == 0 {
//             Ok((status, literal.0.to_string()))
//         } else {
//             parse_au8(
//                 parse_byte(status, au8[0]).map_err(|st| {
//                     Error::from_status(&st, &format!("parsing literal {}", &literal.0))
//                 })?,
//                 &au8[1..],
//                 literal,
//             )
//         }
//     }

//     parse_au8(status, literal.0.as_bytes(), literal)
// }

// #[allow(dead_code)]
// pub(crate) fn parse_literal<'a>(status: Status<'a>, literal: &'a Literal<'a>) -> Result<'a> {
//     let init_tc = (literal.0[0..].as_bytes(), Ok(status));

//     let result_status =
//         tail_call(init_tc, |(pend_lit, acc)| {
//             if pend_lit.len() == 0 {
//                 Ok(TailCall::Return(acc?))
//             } else {
//                 Ok(TailCall::Call((
//                     &pend_lit[1..],
//                     parse_byte(acc?, pend_lit[0]),
//                 )))
//             }
//         }).map_err(|st| Error::from_status(&st, &format!("parsing literal {}", literal.0)))?;

//     Ok((result_status, literal.0.to_string()))
// }

fn parse_byte<'a>(status: Status<'a>, byte: u8) -> result::Result<Status<'a>, Status<'a>> {
    let (b, st) = status.get_byte()?;
    match b == byte {
        true => Ok(st),
        false => Err(st),
    }
}

impl<'a> Status<'a> {
    #[allow(dead_code)]
    fn get_byte(mut self) -> result::Result<(u8, Status<'a>), Status<'a>> {
        if self.text2parse.len() == 0 {
            Err(self)
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
            self.text2parse = &self.text2parse[1..];
            Ok((byte, self))
        }
    }
}

pub fn repeat() {
    let long_input = "a".repeat(10_000_000 + 1);
    let status_init = Status::init(&long_input);
    let str_literal = "a".repeat(5);
    let literal = Literal(&str_literal);

    let mut result = parse_literal(status_init, &literal);
    let mut parsed_size = 0;
    loop {
        match result {
            Ok((status, parsed)) => {
                parsed_size += parsed.len();
                result = parse_literal(status, &literal);
            }
            Err(_) => break,
        }
    }
    println!("parsed_size: {}", parsed_size);
}
