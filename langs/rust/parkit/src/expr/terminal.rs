//! Support for terminal symbols
use crate::status::Result;
use crate::status::Status;

/// This is a simple expression with no dependencies with other rules
#[derive(Debug, PartialEq, Clone)]
pub enum Terminal {
    /// Literal string
    Literal(String),
    ///// Character matches a list of chars or a list of ranges
    //Match(MatchRules),
    ///// Indicates an error.
    ///// It will propagate an error while processing
    //Expected(String),
    /// Any char
    Dot,
    /// End Of File
    Eof,
}

pub(crate) fn parse<'a>(status: Status<'a>, terminal: &Terminal) -> Result<'a> {
    match terminal {
        Terminal::Eof => parse_eof(status),
        Terminal::Literal(l) => parse_literal(status, l),
        Terminal::Dot => parse_dot(status),
    }
}

fn parse_eof<'a>(status: Status<'a>) -> Result<'a> {
    match status.get_char() {
        Ok((st, _ch)) => Err(st.to_error("not end of file :-(")),
        Err(st) => Ok(st),
    }
}

fn parse_literal<'a>(mut status: Status<'a>, literal: &str) -> Result<'a> {
    for ch in literal.chars() {
        status = parse_char(status, ch).map_err(|st| st.to_error(&format!("\"{}\"", literal)))?;
    }
    Ok(status)
}

fn parse_dot<'a>(status: Status<'a>) -> Result<'a> {
    let (status, _ch) = status.get_char().map_err(|st| st.to_error("any char"))?;

    Ok(status)
}

fn parse_char(status: Status, ch: char) -> std::result::Result<Status, Status> {
    let (st, got_ch) = status.get_char()?;
    if ch == got_ch {
        Ok(st)
    } else {
        Err(st)
    }
}

impl<'a> Status<'a> {
    fn get_char(mut self) -> std::result::Result<(Self, char), Self> {
        match self.it_parsing.next() {
            None => Err(self),
            Some(ch) => {
                self.pos.n += 1;
                match ch {
                    '\n' => {
                        self.pos.col = 0;
                        self.pos.row += 1;
                        self.pos.start_line = self.pos.n;
                    }
                    '\r' => {
                        self.pos.col = 0;
                    }
                    _ => {
                        self.pos.col += 1;
                    }
                }
                Ok((self, ch))
            }
        }
    }
}
