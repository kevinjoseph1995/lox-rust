use crate::parser::{LiteralType, Statement};

#[derive(Clone, Debug)]
pub enum Object {
    Number(f64),
    String(Vec<u8>),
    True,
    False,
    Nil,
    Callable(CallableObject),
}

#[derive(Clone, Debug)]
pub struct CallableObject {
    pub name: Vec<u8>,
    pub parameters: Vec<Vec<u8>>,
    pub function_block: Box<Statement>,
    pub parent_environment: *mut EnvironmentNode,
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
            Object::Callable(callable) => {
                write!(f, "fn <{}>", std::str::from_utf8(&callable.name).unwrap())
                // TODO expand a little bit more
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
#[derive(Debug)]
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

pub struct EnvironmentNode {
    environment: Environment,
    parent: *mut EnvironmentNode,
    children: Vec<Box<EnvironmentNode>>,
}

impl EnvironmentNode {
    fn new() -> Self {
        Self {
            environment: Environment::new(),
            parent: std::ptr::null_mut(),
            children: Vec::new(),
        }
    }
}

pub fn add_child(parent: *mut EnvironmentNode) -> *mut EnvironmentNode {
    unsafe {
        (*parent).children.push(Box::new(EnvironmentNode::new()));
        let child = (*parent).children.last_mut().unwrap().as_mut();
        child.parent = parent;
        return child;
    }
}

pub struct EnvironmentManager {
    _global_environment: Box<EnvironmentNode>,
    pub current: *mut EnvironmentNode,
}

impl EnvironmentManager {
    pub fn new() -> Self {
        let mut global_environment = Box::new(EnvironmentNode::new());
        let ptr = global_environment.as_mut() as *mut EnvironmentNode;
        Self {
            _global_environment: global_environment,
            current: ptr,
        }
    }
    fn print_environment_node_helper(&mut self, node: *mut EnvironmentNode, indent: usize) {
        unsafe {
            for var in &(*node).environment.variables {
                print!("{:<width$}", "", width = indent + 2);
                println!(
                    "name: {} value={}",
                    std::str::from_utf8_unchecked(&var.name),
                    var.object
                );
            }
        }
    }

    fn print_environment_tree_helper(&mut self, node: *mut EnvironmentNode, indent: usize) {
        let current = node;
        print!("{:<width$}", "", width = indent);
        println!("{{");
        unsafe {
            self.print_environment_node_helper(current, indent);
            for child in &mut (*current).children {
                let child: *mut EnvironmentNode = child.as_mut();
                self.print_environment_tree_helper(child, indent + 4);
            }
        }
        print!("{:<width$}", "", width = indent);
        println!("}}");
    }
    #[allow(dead_code)]
    pub fn print_environment_tree(&mut self) {
        let current: *mut EnvironmentNode = self._global_environment.as_mut();
        self.print_environment_tree_helper(current, 0);
    }
    #[allow(dead_code)]
    pub fn print_current_environment_node(&mut self) {
        self.print_environment_tree_helper(self.current, 0);
    }

    pub fn lookup(&self, name: &Vec<u8>) -> Option<Object> {
        unsafe {
            let mut current = self.current;
            loop {
                if let Some(value) = (*current).environment.lookup(name) {
                    return Some(value.clone());
                }
                if (*current).parent.is_null() {
                    break;
                }
                current = (*current).parent;
            }
            return None;
        }
    }
    pub fn update(&mut self, name: &Vec<u8>, value: &Object) -> bool {
        unsafe {
            let mut current = self.current;
            loop {
                if let Some(_) = (*current).environment.lookup(name) {
                    (*current).environment.update_or_add(name, value.clone());
                    return true;
                }
                if (*current).parent.is_null() {
                    break;
                }
                current = (*current).parent;
            }
            return false;
        }
    }

    pub fn update_or_add(&mut self, name: &Vec<u8>, value: Object) {
        unsafe {
            (*self.current).environment.update_or_add(name, value);
        }
    }

    pub fn add_callable_object(
        &mut self,
        name: &Vec<u8>,
        parameters: &Vec<Vec<u8>>,
        body: &Box<Statement>,
    ) {
        let callable = CallableObject {
            name: name.clone(),
            parameters: parameters.clone(),
            function_block: body.clone(),
            parent_environment: self.current,
        };
        self.update_or_add(name, Object::Callable(callable));
    }
}
