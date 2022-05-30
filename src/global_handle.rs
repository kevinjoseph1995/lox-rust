use crate::interpreter::Interpreter;
use crate::scanner::Scanner;

pub struct GlobalHandle {
    pub scanner: Scanner,
    pub interpreter: Interpreter,
}

impl GlobalHandle {
    pub fn new() -> GlobalHandle {
        GlobalHandle {
            scanner: Default::default(),
            interpreter: Interpreter {},
        }
    }
}
