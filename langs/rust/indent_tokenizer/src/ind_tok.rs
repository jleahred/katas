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
    pub lines: Vec<String>,
    pub tokens: Vec<Token>,
}


pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut parsing_inf = ParsingTokens::new();

    for l in input.lines() {
        let li = process_line(l);
        parsing_inf = tokenize_line(&li, parsing_inf)?;
    }

    // parsing_inf.prune_current_topic();
    Ok(parsing_inf.get_tokens())
}



fn tokenize_line(line: &Option<LineInfo>, parsing: ParsingTokens) -> Result<ParsingTokens, String> {
    match *line {
        None => Ok(parsing.add_empty_token()),
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
    // fn is_empty(&self) -> bool {
    //     self.lines.is_empty()
    // }
}

#[derive(Debug, Copy, Clone)]
struct PrevLevels {
    indent: u32,
    index: usize,
}

#[derive(Debug)]
struct ParsingTokens {
    prevs: Vec<PrevLevels>,
    tokens_content: Vec<Vec<String>>,
}

impl ParsingTokens {
    fn new() -> ParsingTokens {
        ParsingTokens {
            prevs: Vec::new(),
            tokens_content: Vec::new(), // current_token: Token::new(),
        }
    }

    fn get_tokens(self) -> Vec<Token> {
        let mut result = Vec::new();
        for content in self.tokens_content {
            if content.len() > 0 {
                let mut token = Token::new();
                token.lines = content;
                result.push(token);
            }
        }
        result
    }


    fn add_empty_token(mut self) -> ParsingTokens {
        match self.prevs.last_mut().map(|v| *v) {
            None => {
                self.prevs.push(PrevLevels {
                    indent: 0,
                    index: 0,
                })
            }
            Some(ref mut last_prev) => 
                last_prev.index = last_prev.index + 1
            }
        }
        self.tokens_content.push(Vec::new());
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

        match self.prevs.last().map(|v| *v) {
            None => Ok(self.add_line_new_token_root(l)),
            Some(last_prev) => {
                use std::cmp::Ordering::{Equal, Greater, Less};
                match l.indent.cmp(&last_prev.indent) {
                    Equal => Ok(self.add_line_token_idx(last_prev.index, &l.content)),
                    Greater => Ok(self.add_line_new_token_deep(l)),
                    Less => self.add_line_new_token_shalow(l),
                }
            }
        }
    }

    fn add_line_new_token_root(mut self, l: &LineInfo) -> ParsingTokens {
        let mut new_content = Vec::new();
        new_content.push(l.content.clone());
        self.tokens_content.push(new_content);
        let index = self.prevs.len();
        self.prevs.push(PrevLevels {
            indent: l.indent,
            index: index,
        });
        self
    }

    fn add_line_new_token_deep(mut self, l: &LineInfo) -> ParsingTokens {
        let mut new_content = Vec::new();
        new_content.push(l.content.clone());
        self.tokens_content.push(new_content);
        self
    }

    fn add_line_token_idx(mut self, idx: usize, lcontent: &String) -> ParsingTokens {
        self.tokens_content[idx].push(lcontent.clone());
        self
    }

    fn remove_prev_indents_till(&mut self, pindent: u32) {
        //  look for previous ident level
        while self.prevs.len() > 0 {
            match self.prevs.last().map(|v| *v) {
                None => break,
                Some(PrevLevels { indent, index }) => {
                    if pindent > indent {
                        self.prevs.pop();
                    }
                }
            }
        }
    }

    fn add_line_new_token_shalow(mut self, l: &LineInfo) -> Result<ParsingTokens, String> {
        self.prevs.pop();
        self.remove_prev_indents_till(l.indent);

        match self.prevs.last().map(|v| *v) {
            None => Ok(self.add_line_new_token_root(l)),
            Some(PrevLevels { indent, index }) => {
                use std::cmp::Ordering::{Equal, Greater, Less};
                match indent.cmp(&&l.indent) {
                    Equal => Ok(self.add_line_token_idx(index, &l.content)),
                    Greater => Err("invalid inentation".to_string()),
                    Less => panic!("Indent < new  were removed!!!"),
                }
            }
        }
    }
}
