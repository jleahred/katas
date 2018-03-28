use Error;
use super::Status;

//-----------------------------------------------------------------------
#[allow(dead_code)]
pub(crate) fn parse_literal<'a>(status: Status<'a>, literal: &str) -> Result<Status<'a>, Error> {
    let mut new_status = status.set_parsing_desc(&format!("expected literal {}", literal));

    for ch in literal.chars() {
        new_status = parse_ch(new_status, ch)?
    }
    Ok(new_status)
}

#[test]
fn test_parse_literal_ok() {
    let status_init = Status::init("aaaaaaaaaaaaaaaa");
    let status_end = parse_literal(status_init, "aaa").ok().unwrap();

    assert!(status_end.pos.col == 3);
    assert!(status_end.pos.n == 3);
    assert!(status_end.pos.row == 0);
}

#[test]
fn test_parse_literal_ok2() {
    let status_init = Status::init("abcdefghij");
    let status_end = parse_literal(status_init, "abc").ok().unwrap();

    assert_eq!(status_end.pos.col, 3);
    assert_eq!(status_end.pos.n, 3);
    assert_eq!(status_end.pos.row, 0);
}

#[test]
fn test_parse_literal_fail() {
    let status_init = Status::init("abcdefghij");
    assert!(parse_literal(status_init, "bbb").is_err());
}

#[test]
fn test_parse_literal_fail2() {
    let status_init = Status::init("abcdefghij");
    assert!(parse_literal(status_init, "abd").is_err());
}

//-----------------------------------------------------------------------
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
