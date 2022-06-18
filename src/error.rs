use std::fmt::Display;

#[derive(Debug)]
pub enum LoxError {
    IOError(std::io::Error),
    LexErr(String),
    ParserError(String),
    RuntimeError(String),
    InternalError(String),
}

impl Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxError::IOError(err) => {
                write!(f, "{}", err.to_string())
            }
            LoxError::LexErr(err) => {
                write!(f, "{}", err)
            }
            LoxError::ParserError(err) => {
                write!(f, "{}", err)
            }
            LoxError::RuntimeError(err) => {
                write!(f, "{}", err)
            }
            LoxError::InternalError(err) => {
                write!(f, "{}", err)
            }
        }
    }
}

impl From<std::io::Error> for LoxError {
    fn from(x: std::io::Error) -> Self {
        LoxError::IOError(x)
    }
}

impl From<std::str::Utf8Error> for LoxError {
    fn from(x: std::str::Utf8Error) -> Self {
        LoxError::ParserError(x.to_string())
    }
}

impl From<std::num::ParseFloatError> for LoxError {
    fn from(x: std::num::ParseFloatError) -> Self {
        LoxError::ParserError(x.to_string())
    }
}
