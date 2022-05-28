#[derive(Debug)]
pub enum LoxError {
    IOError(std::io::Error),
}

impl From<std::io::Error> for LoxError {
    fn from(x: std::io::Error) -> Self {
        LoxError::IOError(x)
    }
}
