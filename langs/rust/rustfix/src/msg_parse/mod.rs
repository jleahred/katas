#[cfg(test)]
mod test;

use std::collections::BTreeMap;
use std::collections::LinkedList;

mod errors {
    pub const TAG_INVALID_CHAR: &'static str = "Invalid char in tag.";
}



#[derive(Debug, PartialEq, Eq)]
pub enum ParsingState {
    StReadingTag,
    StReadingValue,
    StFinished,
}

impl Default for ParsingState {
    fn default() -> ParsingState {
        ParsingState::StReadingTag
    }
}


#[derive(Debug, Default, Eq, PartialEq)]
pub struct ParsingInfo {
    pub msg_map: BTreeMap<u32, String>,
    pub body_length: u32,
    pub check_sum: u32,
    pub num_tags: u32,
    pub orig_msg: String,
    pub errors: LinkedList<(u32, String)>, //  position, description
    pub msg_length: u32,

    state: ParsingState,
    reading_tag: String, //  optimization
    reading_value: String, //  optimization */
    current_field_error: Option<(u32, &'static str)>,
}


impl ParsingInfo {
    pub fn new() -> ParsingInfo {
        ParsingInfo { ..Default::default() }
    }

    pub fn add_char(&mut self, ch: char) -> () {
        self.msg_length += 1;
        self.orig_msg.push(transform_ch(ch));
        match self.state {
            ParsingState::StReadingTag => self.add_char_reading_tag(ch),
            ParsingState::StReadingValue => (),
            ParsingState::StFinished => (),
        }
    }

    fn add_char_reading_tag(&mut self, ch: char) {
        self.reading_tag.push(ch);
        match self.current_field_error {
            Some(_) => (),
            None => {
                if ch < '0' || ch > '9' {
                    self.current_field_error = Some((self.msg_length,
                                                     self::errors::TAG_INVALID_CHAR));
                }
            }
        }
    }
}


fn transform_ch(ch: char) -> char {
    if ch == 1u8 as char {
        '^'
    } else {
        ch
    }
}