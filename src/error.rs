#[derive(Debug)]
pub enum LoxError {
    IOError(std::io::Error),
    UTF8Error(std::str::Utf8Error),
    ParseFloatError(std::num::ParseFloatError),
    LexErr(String),
    ParserError(String),
    RuntimeError(String),
}

impl From<std::io::Error> for LoxError {
    fn from(x: std::io::Error) -> Self {
        LoxError::IOError(x)
    }
}

impl From<std::str::Utf8Error> for LoxError {
    fn from(x: std::str::Utf8Error) -> Self {
        LoxError::UTF8Error(x)
    }
}

impl From<std::num::ParseFloatError> for LoxError {
    fn from(x: std::num::ParseFloatError) -> Self {
        LoxError::ParseFloatError(x)
    }
}
