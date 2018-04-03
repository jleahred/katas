//  mod parser::atom

use Error;
use super::Status;
use std::result;

#[cfg(test)]
mod test;

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//
//  T Y P E S
//
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

type Result<'a> = result::Result<Status<'a>, Error>;

pub(crate) struct Literal<'a>(&'a str);
pub(crate) struct Symbol<'a>(&'a str);
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
    let mut new_status = status.set_parsing_desc(&format!("expected literal {}", literal.0));

    for ch in literal.0.chars() {
        new_status = parse_ch(new_status, ch)?;
    }
    Ok(new_status)
}

//-----------------------------------------------------------------------

#[allow(dead_code)]
pub(crate) fn parse_dot<'a>(status: Status<'a>) -> Result<'a> {
    let (_, result_status) = status.set_parsing_desc("expected any char").get_char()?;
    Ok(result_status)
}

//-----------------------------------------------------------------------

#[allow(dead_code)]
pub(crate) fn parse_symbol<'a>(status: Status<'a>) -> Result<'a> {
    let mut new_status = status.set_parsing_desc("expected symbol");

    loop {
        let (got_ch, new_status) = new_status.get_char()?;
        match got_ch {
            'a'...'b' => {}
            _ => return Ok(new_status),
        };
    }
}

//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//  local support

#[allow(dead_code)]
fn parse_ch(status: Status, ch: char) -> Result {
    let (got_ch, new_status) = status.get_char()?;
    if got_ch == ch {
        Ok(new_status)
    } else {
        Err(Error::from_status(&new_status))
    }
}

impl<'a> Status<'a> {
    #[allow(dead_code)]
    fn get_char(mut self) -> result::Result<(char, Status<'a>), Error> {
        let ch = self.t2p_iterator.next().ok_or(Error::from_status(&self))?;
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
        Ok((ch, self))
    }
}
