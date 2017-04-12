// pub fn split_lines(s: &String) -> Lines {
//     s.lines()
// }


// This is the first token
//     This is another token, because it's on a different level
//         And another token
//     This is also a different token
//
// A token can contain
// multiple lines
//     This is another token
//     with three
//     lines
//
// Empty lines can be used to
// separate tokens
//     This is a token,
//     that continues
//     here. Next empty line define
//     a token division
//
//     And this is a different one
//     with a couple of lines



// use std::str::Lines;
use process_line::{LineInfo, process_line};





pub struct Token {
    level: u32,
    lines: Vec<String>,
    tokens: Vec<Token>,
}
impl Token {
    fn create(l: u32) -> Token {
        Token {
            level: l,
            lines: Vec::new(),
            tokens: Vec::new(),
        }
    }
}


struct ParsingTokens {
    prev_indents: Vec<u32>,
    tokens: Vec<Token>, //  list of tokens for this level
}

impl ParsingTokens {
    fn new() -> ParsingTokens {
        ParsingTokens {
            prev_indents: Vec::new(),
            tokens: Vec::new(),
        }
    }
}


pub fn tokenize(input: &String) -> Result<Vec<Token>, String> {
    let mut parsing_inf = ParsingTokens::new();

    for l in input.lines() {
        let li = process_line(l);
        parsing_inf = tokenize_line(&li, parsing_inf)?;
    }

    Ok(Vec::new())
}


fn current_level(parsing: &ParsingTokens) -> u32 {
    match parsing.tokens.last() {
        Some(token) => token.level,
        None => 0,
    }
}

fn add_empty_token_same_level(mut parsing: ParsingTokens) -> ParsingTokens {
    let curr_level = current_level(&parsing);
    parsing.tokens.push(Token::create(curr_level));
    parsing
}

fn add_line_current_token(l: &String, mut parsing: ParsingTokens) -> ParsingTokens {
    match parsing.tokens.last_mut() {
        Some(mut token) => {
            token.lines.push(l.clone());
        }
        None => (),
    }
    parsing
}

fn tokenize_line(line: &Option<LineInfo>, parsing: ParsingTokens) -> Result<ParsingTokens, String> {
    Ok(match *line {
        None => add_empty_token_same_level(parsing),
        Some(ref l) => parsing,
    })
}