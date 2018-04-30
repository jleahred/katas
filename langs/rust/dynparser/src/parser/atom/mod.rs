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
//-----------------------------------------------------------------------
//
//  T Y P E S
//
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------

pub(crate) enum Atom<'a> {
    Literal(Literal<'a>),
    Match(Match),
    Dot,
    EOF,
}

/// contains a &str to the string to parse
pub(crate) struct Literal<'a>(&'a str);

/// contains a string and a Vec<(char,char)>
/// if char matches one in string -> OK
/// if char matches between tuple in vec elems -> OK
pub(crate) struct Match(String, Vec<(char, char)>);

impl Match {
    #[allow(dead_code)]
    pub(crate) fn new() -> Self {
        Match("".to_owned(), vec![])
    }
    #[allow(dead_code)]
    pub(crate) fn with_chars(mut self, chrs: &str) -> Self {
        self.0 = chrs.to_owned();
        self
    }

    #[allow(dead_code)]
    pub(crate) fn with_bound_chars(mut self, bounds: &Vec<(char, char)>) -> Self {
        self.1 = bounds.clone();
        self
    }
}

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
    for ch in literal.0.chars() {
        status = parse_char(status, ch)
            .map_err(|st| Error::from_status(&st, &format!("parsing literal {}", literal.0)))?;
    }
    Ok((status, literal.0.to_string()))
}

#[allow(dead_code)]
pub(crate) fn parse_dot<'a>(status: Status<'a>) -> Result<'a> {
    let (st, ch) = status
        .get_char()
        .map_err(|st| Error::from_status(&st, "parsing dot"))?;

    Ok((st, ch.to_string()))
}

#[allow(dead_code)]
pub(crate) fn parse_match<'a>(status: Status<'a>, match_rules: &Match) -> Result<'a> {
    let match_char = |ch: char| -> bool {
        if match_rules.0.find(ch).is_some() {
            true
        } else {
            for &(b, t) in &match_rules.1 {
                if b <= ch && ch <= t {
                    return true;
                }
            }
            false
        }
    };

    status.take_while(|ch| match_char(ch)).map_err(|st| {
        Error::from_status(
            &st,
            &format!("match. expected {} {:?}", match_rules.0, match_rules.1),
        )
    })
}

#[allow(dead_code)]
pub(crate) fn parse_eof<'a>(status: Status<'a>) -> Result<'a> {
    match status.get_char() {
        Ok((st, _ch)) => Err(Error::from_status(&st, "expected EOF")),
        Err(st) => Ok((st, "".to_owned())),
    }
}

//-----------------------------------------------------------------------
//
//  SUPPORT
//
//-----------------------------------------------------------------------

fn parse_char<'a>(status: Status<'a>, ch: char) -> result::Result<Status<'a>, Status<'a>> {
    let (st, got_ch) = status.get_char()?;
    match ch == got_ch {
        true => Ok(st),
        false => Err(st),
    }
}

impl<'a> Status<'a> {
    #[allow(dead_code)]
    fn get_char(mut self) -> result::Result<(Status<'a>, char), Status<'a>> {
        match self.it_parsing.next() {
            None => Err(self),
            Some(ch) => {
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
                Ok((self, ch))
            }
        }
    }

    #[allow(dead_code)]
    fn take_while<F>(self, fn_check_char: F) -> result::Result<(Status<'a>, String), Status<'a>>
    where
        F: Fn(char) -> bool,
    {
        fn peek_char<'a>(status: &Status<'a>) -> Option<char> {
            status.it_parsing.clone().next()
        }

        let init_tc = (self, "".to_owned());

        let result = tail_call(init_tc, |acc| {
            if peek_char(&acc.0)
                .and_then(|ch| Some(fn_check_char(ch)))
                .unwrap_or(false)
            {
                let (st, ch) = acc.0.get_char()?;
                Ok(TailCall::Call((st, string_with_ch(acc.1, ch))))
            } else {
                Ok(TailCall::Return(acc))
            }
        })?;

        if result.1.len() == 0 {
            Err(result.0)
        } else {
            Ok(result)
        }
    }
}

fn string_with_ch(mut origin: String, ch: char) -> String {
    origin.push(ch);
    origin
}

//-----------------------------------------------------------------------
//  TailCall
//-----------------------------------------------------------------------
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
