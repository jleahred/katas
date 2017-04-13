// TODO:
//      tests
//      types: remove u32 and string
//      try to remove for
//


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




#[derive(Debug)]
pub struct Token {
    // level: u32,
    lines: Vec<String>,
    tokens: Vec<Token>,
}


pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut parsing_inf = ParsingTokens::new();

    for l in input.lines() {
        let li = process_line(l);
        parsing_inf = tokenize_line(&li, parsing_inf)?;
    }

    parsing_inf.prune_current_topic();
    Ok(parsing_inf.tokens)
}



fn tokenize_line(line: &Option<LineInfo>, parsing: ParsingTokens) -> Result<ParsingTokens, String> {
    match *line {
        None => Ok(parsing.add_empty_token_same_level()),
        Some(ref l) => parsing.add_line(l),
    }
}





impl Token {
    fn new() -> Token {
        Token {
            lines: Vec::new(),
            tokens: Vec::new(),
        }
    }
    fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }
}

#[derive(Debug)]
struct ParsingTokens {
    prev_indents: Vec<u32>,
    tokens: Vec<Token>, //  list of tokens for this level
}

impl ParsingTokens {
    fn new() -> ParsingTokens {
        ParsingTokens {
            prev_indents: Vec::new(),
            tokens: Vec::new(), // current_token: Token::new(),
        }
    }

    fn current_token(&mut self) -> &Token {
        if self.tokens.last().is_none() {
            self.tokens.push(Token::new());
        }
        self.tokens.last().unwrap()
    }

    fn current_token_mut(&mut self) -> &mut Token {
        if self.tokens.last().is_none() {
            self.tokens.push(Token::new());
        }
        self.tokens.last_mut().unwrap()
    }

    fn add_empty_token_same_level(mut self) -> ParsingTokens {
        if self.current_token().is_empty() == false {
            self.tokens.push(Token::new());
        }
        self
    }

    fn add_line(self, l: &LineInfo) -> Result<ParsingTokens, String> {
        //  if prev_indents.last().is_none()
        //      create a new token same level
        //  else indent spaces > prev_indents.last()
        //      create a subtoken
        //  if indent spaces == prev_indents.last()
        //      insert on current token
        //  else
        //      create a new token same level

        // let last_indent: Option<u32> = *self.prev_indents.last();

        match self.prev_indents.last().map(|v| *v) {
            None => Ok(self.add_line_new_token_root(l)),
            Some(last_indent) => {
                use std::cmp::Ordering::{Equal, Greater, Less};
                match l.indent.cmp(&last_indent) {
                    Equal => Ok(self.add_line_current_token(&l.content)),
                    Greater => Ok(self.add_line_new_token_deep(l)),
                    Less => self.add_line_new_token_shalow(l),
                }
            }
        }
    }

    fn prune_current_topic(&mut self) {
        if self.current_token().is_empty() {
            self.tokens.pop();
        }
    }

    fn prepare_add_line_new_token(&mut self, l: &LineInfo) -> Token {
        self.prune_current_topic();

        self.prev_indents.push(l.indent);

        let mut new_token = Token::new();
        new_token.lines.push(l.content.clone());
        new_token
    }

    fn add_line_new_token_root(mut self, l: &LineInfo) -> ParsingTokens {
        let new_token = self.prepare_add_line_new_token(l);
        self.tokens.push(new_token);
        self
    }

    fn add_line_new_token_deep(mut self, l: &LineInfo) -> ParsingTokens {
        let new_token = self.prepare_add_line_new_token(l);
        self.current_token_mut().tokens.push(new_token);
        self
    }

    fn add_line_current_token(mut self, lcontent: &String) -> ParsingTokens {
        self.current_token_mut().lines.push(lcontent.clone());
        self
    }

    fn remove_prev_indents_till(&mut self, indent: u32) {
        //  look for previous ident level
        while self.prev_indents.len() > 0 {
            match self.prev_indents.last().map(|v| *v) {
                None => break,
                Some(pindent) => {
                    if pindent > indent {
                        self.prev_indents.pop();
                        ()
                    }
                }
            }
        }
    }

    fn add_line_new_token_shalow(mut self, l: &LineInfo) -> Result<ParsingTokens, String> {
        self.prev_indents.pop();
        self.remove_prev_indents_till(l.indent);

        match self.prev_indents.last().map(|v| *v) {
            None => Ok(self.add_line_new_token_root(l)),
            Some(prev_ind) => {
                use std::cmp::Ordering::{Equal, Greater, Less};
                match prev_ind.cmp(&&l.indent) {
                    Equal => Ok(self.add_line_current_token(&l.content)),
                    Greater => Err("invalid inentation".to_string()),
                    Less => panic!("Indent < new  were removed!!!"),
                }
            }
        }
    }
}
