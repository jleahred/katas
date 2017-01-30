#[cfg(test)]
mod test;

use std::collections::BTreeMap;
use std::collections::LinkedList;

mod errors {
    pub const TAG_INVALID_CHAR: &'static str = "Invalid char in tag";
    pub const TAG_TOO_LONG: &'static str = "Tag too long";
    pub const VAL_TOO_LONG: &'static str = "Tag too long";
}


const TAG_LONG_LIMIT: usize = 10;
const VAL_LONG_LIMIT: usize = 50;


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
    pub errors: LinkedList<(u32, &'static str)>, //  position, description
    pub msg_length: u32,

    state: ParsingState,
    reading_tag: String, //  optimization
    reading_val: String, //  optimization */
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
            ParsingState::StReadingValue => self.add_char_reading_val(ch),
            ParsingState::StFinished => (),
        }
    }

    fn add_char_reading_tag(&mut self, ch: char) {
        match self.current_field_error {
            Some(_) => (),
            None => {
                if ch >= '0' && ch <= '9' {
                    self.reading_tag.push(ch);
                    if self.reading_tag.len() + 1 > TAG_LONG_LIMIT {
                        self.current_field_error = Some((self.msg_length,
                                                         self::errors::TAG_TOO_LONG));
                    }
                } else if ch == '=' {
                    self.state = ParsingState::StReadingValue;
                } else {
                    self.current_field_error = Some((self.msg_length,
                                                     self::errors::TAG_INVALID_CHAR));
                    self.reading_tag.push(ch);
                }
            }
        }
    }

    fn add_char_reading_val(&mut self, ch: char) {
        match self.current_field_error {
            Some(_) => (),
            None => {
                if ch == 1u8 as char {
                    self.process_tag_val();
                } else {
                    self.reading_val.push(ch);
                    if self.reading_val.len() + 1 > VAL_LONG_LIMIT {
                        self.current_field_error = Some((self.msg_length,
                                                         self::errors::VAL_TOO_LONG));
                    }
                }
            }
        }
    }

    fn process_tag_val(&mut self) {
        self.reading_tag = "".to_string();
        self.reading_val = "".to_string();
        match self.current_field_error {
            Some(error) => self.errors.push_back(error),
            None => (),
        }
    }
}


fn transform_ch(ch: char) -> char {
    if ch == 1u8 as char { '^' } else { ch }
}