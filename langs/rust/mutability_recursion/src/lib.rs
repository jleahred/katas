pub mod parser;

#[derive(PartialEq, Clone, Debug)]
pub struct Possition {
    /// char position parsing
    pub n: usize,
    /// row parsing row
    pub row: usize,
    /// parsing col
    pub col: usize,
}

impl Possition {
    #[allow(dead_code)]
    fn init() -> Self {
        Self {
            n: 0,
            row: 0,
            col: 0,
        }
    }
}

// #[derive(Debug, PartialEq, Clone)]
pub struct Error {
    pub pos: Possition,
    pub descr: String,
    pub line: String,
}

