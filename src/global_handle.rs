use crate::scanner::Scanner;

pub struct GlobalHandle {
    pub scanner: Scanner,
}

impl GlobalHandle {
    pub fn new() -> GlobalHandle {
        GlobalHandle {
            scanner: Default::default(),
        }
    }
}
