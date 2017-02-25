#[cfg(test)]
mod test;

use std::collections::BTreeMap;
use std::collections::LinkedList;

mod errors {
    pub const TAG_INVALID_CHAR: &'static str = "Invalid char in tag";
    pub const TAG_TOO_LONG: &'static str = "Tag too long";
    pub const VAL_TOO_LONG: &'static str = "Value too long";
}

mod tags {
    pub const BEGIN_STRING: u32 = 8;
    pub const BODY_LENGTH: u32 = 9;
    pub const CHECK_SUM: u32 = 10;
    pub const MESSAGE_TYPE: u32 = 35;
}



const TAG_MAX_VALUE: u32 = 1_000_000;
const VAL_LONG_LIMIT: usize = 50;


#[derive(Debug, PartialEq, Eq, Clone)]
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
pub struct Parsed {
    pub msg_map: BTreeMap<u32, String>,
    pub body_length: usize,
    pub check_sum: u16,
    pub num_tags: u32,
    pub orig_msg: String,
    pub errors: LinkedList<(u32, &'static str)>, //  position, description
    pub msg_length: u32,
}


#[derive(Debug, Default, Eq, PartialEq)]
pub struct ParsingInfo {
    pub parsed: Parsed,

    state: ParsingState,
    reading_tag: u32, //  optimization
    reading_tag_len: usize, //  optimization
    reading_val: String, //  optimization
    reading_checksum: u16,

    current_field_error: Option<(u32, &'static str)>,
}


pub fn add_char(mut pi: ParsingInfo, ch: char) -> ParsingInfo {
    pi.parsed.msg_length += 1;
    pi.parsed.orig_msg.push(tranf_ch_01(ch));

    pi.reading_checksum += ch as u16;
    pi.reading_checksum %= 256;

    match pi.state {
        ParsingState::StReadingTag => pi.add_char_reading_tag(ch),
        ParsingState::StReadingValue => pi.add_char_reading_val(ch),
        ParsingState::StFinished => {
            pi = ParsingInfo::new();
            pi = add_char(pi, ch)
        }
    }
    pi
}

impl ParsingInfo {
    pub fn new() -> ParsingInfo {
        ParsingInfo { ..Default::default() }
    }

    fn add_char_reading_tag(&mut self, ch: char) {
        match self.current_field_error {
            Some(_) => (),
            None => {
                if ch >= '0' && ch <= '9' {
                    self.reading_tag *= 10;
                    self.reading_tag += ch as u32 - '0' as u32;
                    self.reading_tag_len += 1;
                    if self.reading_tag > TAG_MAX_VALUE {
                        self.current_field_error = Some((self.parsed.msg_length,
                                                         self::errors::TAG_TOO_LONG));
                    }
                } else if ch == '=' {
                    self.state = ParsingState::StReadingValue;
                } else {
                    self.current_field_error = Some((self.parsed.msg_length,
                                                     self::errors::TAG_INVALID_CHAR));
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
                        self.current_field_error = Some((self.parsed.msg_length,
                                                         self::errors::VAL_TOO_LONG));
                    }
                }
            }
        }
    }

    fn process_tag_val(&mut self) {
        match self.current_field_error {
            Some(error) => self.parsed.errors.push_back(error),
            None => (),
        }

        self.parsed.msg_map.insert(self.reading_tag, self.reading_val.clone());

        if self.reading_tag != tags::CHECK_SUM {
            self.parsed.check_sum = self.reading_checksum;
        }
        self.update_body_length();


        self.state = if self.reading_tag == tags::CHECK_SUM {
            ParsingState::StFinished
        } else {
            ParsingState::StReadingTag
        };

        self.reading_tag = 0;
        self.reading_tag_len = 0;
        self.reading_val = "".to_string();
    }

    fn update_body_length(&mut self) {
        let tag = self.reading_tag;
        // Body length[edit]
        // Body length is the character count starting at tag 35 (included) all the way to tag 10 (excluded). SOH delimiters do count in body length.
        // For Example: (SOH have been replaced by'|')
        //
        // 8=FIX.4.2|9=65|35=A|49=SERVER|56=CLIENT|34=177|52=20090107-18:15:16|98=0|108=30|10=062|
        // 0   + 0  + 5  +   10    +   10    +  7   +        21          + 5  +  7   +   0    = 65
        // Has a Body length of 65.
        // The SOH delimiter at the end of a Tag=Value belongs to the Tag.
        //
        if tag != tags::BEGIN_STRING && tag != tags::BODY_LENGTH && tag != tags::CHECK_SUM {
            self.parsed.body_length += self.reading_tag_len + self.reading_val.len() + 2;
        }
    }
}


fn tranf_ch_01(ch: char) -> char {
    if ch == 1u8 as char {
        '|'
    } else {
        ch
    }
}
