//  mod parser::atom

use Error;
use super::Status;

#[cfg(test)]
mod test;

//-----------------------------------------------------------------------
//  TYPES
//-----------------------------------------------------------------------

pub(crate) struct Literal<'a>(&'a str);
// pub(crate) struct Match(String, Vec<(char, char)>);
// pub(crate) struct Symbol(String);

//-----------------------------------------------------------------------
//  API
//-----------------------------------------------------------------------
// #[allow(dead_code)]
// fn lit(txt: &str) -> Literal {
//     Literal(txt.to_owned())
// }

#[allow(dead_code)]
pub(crate) fn parse_literal<'a>(
    status: Status<'a>,
    literal: &Literal,
) -> Result<Status<'a>, Error> {
    let mut new_status = status.set_parsing_desc(&format!("expected literal {}", literal.0));

    //  todo: pending try_fold
    for ch in literal.0.chars() {
        new_status = parse_ch(new_status, ch)?;
    }
    Ok(new_status)
}

#[allow(dead_code)]
pub(crate) fn parse_dot<'a>(status: Status<'a>) -> Result<Status<'a>, Error> {
    let (_, result_status) = status
        .set_parsing_desc(&format!("expected any char"))
        .next()?;
    Ok(result_status)
}

//-----------------------------------------------------------------------
//  local support
#[allow(dead_code)]
fn parse_ch(status: Status, ch: char) -> Result<Status, Error> {
    let (got_ch, new_status) = status.next()?;
    if got_ch == ch {
        Ok(new_status)
    } else {
        Err(Error::from_status(&new_status))
    }
}

impl<'a> Status<'a> {
    #[allow(dead_code)]
    fn next(mut self) -> Result<(char, Status<'a>), Error> {
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
