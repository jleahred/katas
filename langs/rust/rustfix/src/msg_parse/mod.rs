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
    // pub const MESSAGE_TYPE: u32 = 35;
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


#[derive(Debug, Default, Eq, PartialEq)]
pub struct Parsing {
    parsed: Parsed,

    state: ParsingState,
    reading_tag: u32, //  optimization
    reading_tag_len: usize, //  optimization
    reading_val: String, //  optimization
    reading_checksum: u16,

    current_field_error: Option<(u32, &'static str)>,

    errors: LinkedList<(u32, &'static str)>, //  position, description
}



pub enum ParsingResult {
    Parsing(Parsing),
    ParsedOK(Parsed),
    ParsedErrors {
        parsed: Parsed,
        errors: LinkedList<(u32, &'static str)>,
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

    fn set_current_field_error(&mut self, error: (u32, &'static str)) {
        self.current_field_error = Some(error);
        self.state = ParsingState::StReadingValue;
    }

    fn add_char_reading_tag(mut self, ch: char) -> Parsing {
        if ch == 1u8 as char {
            let error = (self.parsed.msg_length, self::errors::TAG_INVALID_CHAR);
            self.errors.push_back(error);
        } else {
            if ch >= '0' && ch <= '9' {
                self.reading_tag *= 10;
                self.reading_tag += ch as u32 - '0' as u32;
                self.reading_tag_len += 1;
                if self.reading_tag > TAG_MAX_VALUE {
                    let error = (self.parsed.msg_length, self::errors::TAG_TOO_LONG);
                    self.set_current_field_error(error);
                }
            } else if ch == '=' {
                if self.reading_tag == 0 {
                    let error = (self.parsed.msg_length, self::errors::TAG_INVALID_CHAR);
                    self.set_current_field_error(error);
                }
                self.state = ParsingState::StReadingValue;
            } else {
                let error = (self.parsed.msg_length, self::errors::TAG_INVALID_CHAR);
                self.set_current_field_error(error);
            }
        }
        self
    }

    fn add_char_reading_val(mut self, ch: char) -> ParsingResult {
        //  end of field
        if ch == 1u8 as char {
            self.process_tag_val();

            //  last tag
            if self.reading_tag == tags::CHECK_SUM {
                match self.current_field_error {
                    Some(error) => {
                        self.errors.push_back(error);

                        ParsingResult::ParsedErrors {
                            parsed: self.parsed,
                            errors: self.errors,
                        }
                    }
                    None => ParsingResult::ParsedOK(self.parsed),
                }
            } else {
                self.reset_reading_tag();
                ParsingResult::Parsing(self)
            }
        } else {
            //  not end of field
            match self.current_field_error {
                None => {
                    self.reading_val.push(ch);
                    if self.reading_val.len() + 1 > VAL_LONG_LIMIT {
                        let error = (self.parsed.msg_length, self::errors::VAL_TOO_LONG);
                        self.set_current_field_error(error);
                    }
                }
                Some(_) => (),
            }
            ParsingResult::Parsing(self)
        }
    }

    fn process_tag_val(&mut self) {
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
