#[cfg(test)]
pub mod test;


use std::collections::BTreeMap;
use std::collections::LinkedList;


#[derive(Debug)]
pub struct Parsed {
    pub msg_map: BTreeMap<u32, String>,
    pub body_length: u32,
    pub check_sum: u32,
    pub num_tags: u32,
    pub orig_msg: String,
    pub errors: LinkedList<(u32, String)>,
    pub msg_length: u32,
}


impl Parsed {
    fn add_char(mut self, ch: char) -> () {
        let repl_ch = if ch == 1u8 as char {
            '^'
        } else {
            ch
        };
        self.msg_length += 1;
        // self.curr_state_length += 1;
        self.orig_msg.push(repl_ch);
    }
}




#[derive(Debug)]
pub enum ParsingState {
    StReadingTag {
        tag: String,
    },
    StReadingValue {
        tag: String,
        value: String,
    },
    StFinished,
}

impl ParsingState {
    pub fn add_char(self, ch: char) -> ParsingState {
        match self {
            ParsingState::StReadingTag { mut tag } => {
                ParsingState::StReadingTag {
                    tag: {
                        tag.push(ch);
                        tag
                    },
                }
            }
            ParsingState::StReadingValue { tag, mut value } => {
                ParsingState::StReadingValue {
                    tag: tag,
                    value: {
                        value.push(ch);
                        value
                    },
                }
            }
            ParsingState::StFinished => ParsingState::StReadingTag { tag: String::new() },
        }
    }
}



pub struct ParsingInfo {
    parsing_state: ParsingState,
    parsed: Parsed,
}



impl ParsingInfo {
    pub fn add_char(self, ch: char) -> () {
        self.parsed.add_char(ch);
        self.parsing_state.add_char(ch);
    }
}
