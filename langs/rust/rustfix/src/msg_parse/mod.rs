#[cfg(test)]
mod test;

use std::cmp::min;
use std::collections::BTreeMap;
use std::collections::LinkedList;

mod errors {
    pub const TAG_INVALID_CHAR: &'static str = "Invalid char in tag";
    pub const TAG_TOO_LONG: &'static str = "Tag too long";
    pub const VAL_TOO_LONG: &'static str = "Value too long";
    pub const TAG_INVALID_POS: &'static str = "Tag invalid pos";
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
enum ParsingState {
    StReadingTag,
    StReadingValue,
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
    pub msg_length: u32,
}


#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub struct ErrorInf {
    pub pos: u32,
    pub message: &'static str,
    pub context: String,
}


#[derive(Debug, Default, Eq, PartialEq)]
pub struct Parsing {
    parsed: Parsed,

    state: ParsingState,
    reading_tag: u32, //  optimization
    reading_tag_len: usize, //  optimization
    reading_val: String, //  optimization
    reading_checksum: u16,

    current_field_error: Option<ErrorInf>,

    errors: LinkedList<ErrorInf>, //  position, description
}



pub enum ParsingResult {
    Parsing(Parsing),
    ParsedOK(Parsed),
    ParsedErrors {
        parsed: Parsed,
        errors: LinkedList<ErrorInf>,
    }, //  position, description,
}


pub fn add_char(mut parsing: Parsing, ch: char) -> ParsingResult {
    parsing.parsed.msg_length += 1;
    parsing.parsed.orig_msg.push(tranf_ch_01(ch));

    parsing.reading_checksum += ch as u16;
    parsing.reading_checksum %= 256;

    match parsing.state {
        ParsingState::StReadingTag => ParsingResult::Parsing(parsing.add_char_reading_tag(ch)),
        ParsingState::StReadingValue => parsing.add_char_reading_val(ch),
    }
}

impl Parsing {
    pub fn new() -> Parsing {
        Parsing { ..Default::default() }
    }

    fn reset_reading_tag(&mut self) {
        self.reading_tag = 0;
        self.reading_tag_len = 0;
        self.reading_val.clear();
    }

    fn set_current_field_error(&mut self, msg: &'static str) {
        match &mut self.current_field_error {
            &mut Some(_) => (),
            &mut None => {
                self.current_field_error = Some(self.get_error_inf(msg));
            }
        }
    }


    fn get_error_inf(&self, msg: &'static str) -> ErrorInf {
        let back = min(7, self.parsed.orig_msg.len());
        let context: String = self.parsed.orig_msg[self.parsed.orig_msg.len() - back..].to_string();
        ErrorInf {
            pos: self.parsed.msg_length,
            message: msg,
            context: format!("parsing... {:?}", context),
        }
    }

    fn add_char_reading_tag(mut self, ch: char) -> Parsing {
        if ch == 1u8 as char {
            self.reset_reading_tag();
            let error = self.get_error_inf(errors::TAG_INVALID_CHAR);
            self.errors.push_back(error);
            self.update_body_length();
        } else if ch >= '0' && ch <= '9' {
            //  if error reading tag, ignore res
            match self.current_field_error {
                Some(_) => (),
                None => {
                    if self.reading_tag > TAG_MAX_VALUE {
                        self.set_current_field_error(errors::TAG_TOO_LONG);
                    } else {
                        self.reading_tag *= 10;
                        self.reading_tag += ch as u32 - '0' as u32;
                        self.reading_tag_len += 1;
                    }
                }
            }
        } else if ch == '=' {
            if self.reading_tag == 0 {
                self.set_current_field_error(errors::TAG_INVALID_CHAR);
                self.state = ParsingState::StReadingValue;
            }
            self.state = ParsingState::StReadingValue;
        } else {
            self.set_current_field_error(errors::TAG_INVALID_CHAR);
        }
        self
    }

    fn add_char_reading_val(mut self, ch: char) -> ParsingResult {
        match &self.current_field_error {
            &Some(ref error) => {
                self.errors.push_back(error.clone());
            }
            &None => (),
        }
        self.current_field_error = None;

        //  end of field
        if ch == 1u8 as char {
            self.process_tag_val();

            //  last tag
            if self.reading_tag == tags::CHECK_SUM {
                if self.errors.is_empty() {
                    ParsingResult::ParsedOK(self.parsed)
                } else {
                    ParsingResult::ParsedErrors {
                        parsed: self.parsed,
                        errors: self.errors,
                    }
                }
            } else {
                self.reset_reading_tag();
                ParsingResult::Parsing(self)
            }
        } else {
            //  not end of field
            if self.reading_val.len() + 1 > VAL_LONG_LIMIT {
                self.set_current_field_error(errors::VAL_TOO_LONG);
            } else {
                self.reading_val.push(ch);

            }
            ParsingResult::Parsing(self)
        }
    }

    fn check_tag_pos(&mut self) {
        if self.parsed.num_tags == 1 && self.reading_tag != tags::BEGIN_STRING {
            let error = self.get_error_inf(errors::TAG_INVALID_POS);
            self.errors.push_back(error);
        } else if self.parsed.num_tags == 2 && self.reading_tag != tags::BODY_LENGTH {
            let error = self.get_error_inf(errors::TAG_INVALID_POS);
            self.errors.push_back(error);
        }
        if self.parsed.num_tags == 3 && self.reading_tag != tags::MESSAGE_TYPE {
            let error = self.get_error_inf(errors::TAG_INVALID_POS);
            self.errors.push_back(error);
        }
    }

    fn process_tag_val(&mut self) {
        self.parsed.num_tags += 1;
        self.check_tag_pos();
        self.parsed.msg_map.insert(self.reading_tag, self.reading_val.clone());
        if self.reading_tag != tags::CHECK_SUM {
            self.parsed.check_sum = self.reading_checksum;
        }
        self.update_body_length();
        self.state = ParsingState::StReadingTag;
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
