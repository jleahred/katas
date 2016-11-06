#[cfg(test)]
pub mod test;


use std::collections::BTreeMap;
use std::collections::LinkedList;


#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum ParsingState {
    ReadingTag,
    ReadingValue,
    Finished,
}

impl Default for ParsingState {
    fn default() -> ParsingState {
        ParsingState::ReadingTag
    }
}


#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct Parsing {
    pub parsing_state: ParsingState,
    pub msg_map: BTreeMap<u32, String>,
    pub body_length: u32,
    pub check_sum: u32,
    pub num_tags: u32,
    pub orig_msg: String,
    pub errors: LinkedList<(u32, String)>,
    pub msg_length: u32,
    pub curr_state_length: u32,
    pub curr_tag_string: String,
    pub curr_string: String,
}


impl Parsing {
    pub fn add_char(&mut self, ch: char) -> () {
        self.increase_counters(ch);
        match self.parsing_state.clone() {
            ParsingState::ReadingTag => self.add_char_reading_tag(ch),
            ParsingState::ReadingValue => self.add_char_reading_value(ch),
            ParsingState::Finished => self.add_char_reading_tag(ch),
        };
    }

    fn increase_counters(&mut self, ch: char) -> () {
        let repl_ch = if ch == 1u8 as char {
            '^'
        } else {
            ch
        };
        self.msg_length += 1;
        self.curr_state_length += 1;
        self.orig_msg.push(repl_ch);
    }

    fn add_char_reading_tag(&mut self, ch: char) -> () {
        if ch == '=' {
            self.parsing_state = ParsingState::ReadingValue;
            self.curr_state_length = 0;
            self.curr_tag_string = self.curr_string.clone();
        } else {
            self.curr_string.push(ch);
        }
    }

    fn add_char_reading_value(&mut self, ch: char) -> () {
        if ch == '\u{1}' {
            self.parsing_state = ParsingState::Finished;
            self.curr_state_length = 0;
            self.curr_tag_string = String::new();
        } else {
            self.curr_string.push(ch);
        }
    }
}
