use std::fmt::Display;

use crate::runtime::Object;

#[derive(Debug)]
pub enum LoxError {
    IOError(std::io::Error),
    LexErr,
    ParserError(String),
    RuntimeError(String),
    InternalError(String),
    Return(Object),
}

impl Display for LoxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxError::IOError(err) => {
                write!(f, "IOError {}", err.to_string())
            }
            LoxError::LexErr => {
                write!(f, "Lexer Error")
            }
            LoxError::ParserError(err) => {
                write!(f, "ParserError {}", err)
            }
            LoxError::RuntimeError(err) => {
                write!(f, "RuntimeError {}", err)
            }
            LoxError::Return(value) => {
                write!(f, "{}", value)
            }
            LoxError::InternalError(err) => write!(f, "InternalError {}", err),
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
