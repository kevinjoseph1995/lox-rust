use crate::parser::{LiteralType, Statement};

#[derive(Clone)]
pub enum Object {
    Number(f64),
    String(Vec<u8>),
    True,
    False,
    Nil,
    Callable(Vec<Vec<u8>>, Box<Statement>),
}

impl From<LiteralType> for Object {
    fn from(literal: LiteralType) -> Self {
        match literal {
            LiteralType::Number(number) => Object::Number(number),
            LiteralType::String(string) => Object::String(string),
            LiteralType::True => Object::True,
            LiteralType::False => Object::False,
            LiteralType::Nil => Object::Nil,
        }
    }
}

impl From<bool> for Object {
    fn from(v: bool) -> Object {
        if v {
            Object::True
        } else {
            Object::False
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        if let (Object::Number(value1), Object::Number(value2)) = (&self, &other) {
            return value1 == value2;
        }
        if let (Object::String(value1), Object::String(value2)) = (&self, &other) {
            return value1 == value2;
        }
        return self == other;
    }
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Number(number) => {
                write!(f, "{}", number)
            }
            Object::String(u8_vec) => {
                let result = std::str::from_utf8(u8_vec);

                match result {
                    Ok(str_val) => write!(f, "{}", str_val),
                    Err(_) => write!(f, "UTF decoding error"),
                }
            }
            Object::True => {
                write!(f, "True")
            }
            Object::False => {
                write!(f, "False")
            }
            Object::Nil => {
                write!(f, "Nil")
            }
            Object::Callable(_, _) => {
                write!(f, "Callable object") // TODO expand a little bit more
            }
        }
    }
}

pub fn is_true_value(value: &Object) -> bool {
    match value {
        Object::True => true,
        Object::Number(num) => {
            if num.clone() != 0.0f64 {
                return true;
            }
            false
        }
        Object::String(_) => true,
        _ => false,
    }
}

struct NamedObject {
    name: Vec<u8>,
    object: Object,
}

struct Environment {
    variables: Vec<NamedObject>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            variables: Vec::new(),
        }
    }

    fn name_comp(first: &Vec<u8>, second: &Vec<u8>) -> bool {
        if first.len() != second.len() {
            return false;
        }

        for (ch1, ch2) in first.iter().zip(second.iter()) {
            if ch1 != ch2 {
                return false;
            }
        }
        return true;
    }

    fn lookup(&self, name: &Vec<u8>) -> Option<&Object> {
        if let Some(name_value_pair) = self
            .variables
            .iter()
            .find(|x| Self::name_comp(&(*x).name, name))
        {
            return Some(&name_value_pair.object);
        }
        return None;
    }

    fn update_or_add(&mut self, name: &Vec<u8>, value: Object) {
        if let Some(name_value_pair) = self
            .variables
            .iter_mut()
            .find(|x| Self::name_comp(&(*x).name, &name))
        {
            name_value_pair.object = value
        } else {
            self.variables.push(NamedObject {
                name: name.clone(),
                object: value,
            });
        }
    }
}

pub type NodeID = usize;
struct EnvironmentNode {
    environment: Environment,
    parent_id: Option<NodeID>,
    children: Vec<NodeID>,
}

pub struct EnvironmentManager {
    environment_arena: Vec<EnvironmentNode>,
    environment_stack: Vec<NodeID>,
}

impl EnvironmentManager {
    pub fn new() -> Self {
        EnvironmentManager {
            environment_arena: vec![EnvironmentNode {
                environment: Environment::new(),
                parent_id: None,
                children: Vec::new(),
            }],
            environment_stack: vec![0],
        }
    }

    pub fn push_context(&mut self) {
        let child_id = self.create_child(self.environment_stack.last().unwrap().clone());
        self.environment_stack.push(child_id);
    }

    pub fn pop_context(&mut self) {
        assert!(
            self.environment_stack.len() > 1,
            "Global environment should never be popped out"
        );
        self.environment_stack.pop();
    }

    fn create_child(&mut self, id: NodeID) -> NodeID {
        let child_id = self.environment_arena.len();
        self.environment_arena[id].children.push(child_id);
        self.environment_arena.push(EnvironmentNode {
            environment: Environment::new(),
            parent_id: Some(id),
            children: Vec::new(),
        });

        return self.environment_arena.len() - 1;
    }

    pub fn lookup(&mut self, name: &Vec<u8>) -> Option<&Object> {
        let mut current_id = self.environment_stack.last().unwrap().clone();

        loop {
            if let Some(value) = self.environment_arena[current_id].environment.lookup(name) {
                return Some(value);
            }
            if let Some(parent_id) = self.environment_arena[current_id].parent_id {
                current_id = parent_id;
            } else {
                // Reached the root of our tree
                break;
            }
        }
        None
    }
    pub fn update_or_add(&mut self, name: &Vec<u8>, value: Object) {
        let id = self.environment_stack.last().unwrap().clone();
        self.environment_arena[id]
            .environment
            .update_or_add(name, value)
    }

    pub fn update(&mut self, name: &Vec<u8>, value: &Object) -> bool {
        let id = self.environment_stack.last().unwrap().clone();

        let mut current_id = id;
        loop {
            let search_result = self.environment_arena[current_id]
                .environment
                .variables
                .iter_mut()
                .find(|x| Environment::name_comp(&(*x).name, &name));
            if let Some(named_object) = search_result {
                named_object.object = value.clone();
                return true;
            } else {
                if let Some(parent_id) = self.environment_arena[current_id].parent_id {
                    current_id = parent_id;
                } else {
                    return false;
                }
            }
        }
    }
}
