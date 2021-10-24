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

impl Error {
    pub(crate) fn from_status(status: &super::status::Status, expected: &str) -> Self {
        Error {
            pos: status.pos.clone(),
            expected: im::vector![expected.to_owned()],
        }
    }
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
