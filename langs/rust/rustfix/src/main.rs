extern crate rustfix;

use rustfix::msg_parse::{ParsingResult, Parsing, add_char};


pub const MSG_TEST_WIKIPEDIA1: &'static str = "8=FIX.4.2|9=178|35=8|49=PHLX|56=PERS|52=20071123-05:\
                                               30:00.000|11=ATOMNOCCC9990900|20=3|150=E|39=E|55=MSFT|167=CS|54=1|38=15|40=2|44=15|58=PHLX \
                                               EQUITY TESTING|59=0|47=C|32=0|31=0|151=15|14=0|6=0|10=128|";



fn add_chars_full_message(mut parser: Parsing, s: &'static str) -> ParsingResult {
    for ch in s.chars() {
        match add_char(parser, conv_separator(ch)) {
            ParsingResult::Parsing(parsing) => parser = parsing,
            ParsingResult::ParsedErrors { parsed: p, errors: e } => {
                return ParsingResult::ParsedErrors {
                    parsed: p,
                    errors: e,
                };
            }
            ParsingResult::ParsedOK(parsed) => {
                return ParsingResult::ParsedOK(parsed);
            }
        }
    }
    panic!("No full message processed")
}

fn conv_separator(ch: char) -> char {
    if ch == '^' || ch == '|' {
        1u8 as char
    } else {
        ch
    }
}



pub fn main() {
    for i in 0..1_000_000 {
        let parser = Parsing::new();

        match add_chars_full_message(parser, MSG_TEST_WIKIPEDIA1) {
            ParsingResult::ParsedOK(_) => {
                if i % 1000 == 0 {
                    () //print!(".")
                }
            }
            ParsingResult::Parsing(_) => panic!("Incomplete parsing on full message"),
            ParsingResult::ParsedErrors { parsed, errors } => {
                panic!("Error parsing {:?}, {:?}", parsed, errors)
            }
        };
    }
    println!("#");
}