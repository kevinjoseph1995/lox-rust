use crate::interpreter::Interpreter;

pub struct GlobalHandle {
    pub interpreter: Interpreter,
}

impl GlobalHandle {
    pub fn new() -> GlobalHandle {
        GlobalHandle {
            interpreter: Interpreter::new(),
        }
    }
}
