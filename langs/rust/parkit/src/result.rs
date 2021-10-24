//! Result type for this crate

/// Result type for parse function
pub type Result<'a> = std::result::Result<(), Error>;

/// Context error information
#[derive(Debug, Clone)]
pub struct Error {
    /// Possition achive parsing
    pub pos: Possition,
    /// error description
    pub expected: im::Vector<String>,
    /// full line on error
    pub line: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let expected_string: String = self
            .expected
            .iter()
            .map(|s| format!("    {}\n", s))
            .collect();
        let line = self.line.clone();

        write!(
            f,
            r#"
Error parsing line:

    {}> {}
       {}^{}
Expected:

{}

"#,
            self.pos.row + 1,
            line,
            std::iter::repeat("~")
                .take(std::cmp::max(self.pos.col as i32 - 1, 0) as usize)
                .collect::<String>(),
            std::iter::repeat("~")
                .take(std::cmp::max(line.chars().count() - self.pos.col, 0) as usize)
                .collect::<String>(),
            expected_string
        )
        // write!(f, "Expected:\n")
    }
}

/// Position information (used in case of Error)
#[derive(PartialEq, Clone, Debug)]
pub struct Possition {
    /// number of chars for position
    pub n: usize,
    /// row of possition
    pub row: usize,
    /// col of possition
    pub col: usize,
    /// line started on... for current possition
    pub start_line: usize,
}

impl Possition {
    pub(crate) fn init() -> Self {
        Possition {
            n: 0,
            row: 0,
            col: 0,
            start_line: 0,
        }
    }
}
