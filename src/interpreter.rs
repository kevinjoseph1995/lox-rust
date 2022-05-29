use crate::scanner::Scanner;
pub struct Interpreter {
    pub scanner: Scanner,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            scanner: Default::default(),
        }
    }
}
