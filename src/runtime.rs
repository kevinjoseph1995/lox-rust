use crate::parser::{LiteralType, Statement};
use core::fmt::Debug;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Formatter,
    rc::{Rc, Weak},
};

//////////////////////////////////////////////LoxClass/////////////////////////////////////////
#[derive(Clone)]
pub struct LoxClass {
    pub name: String,
    pub methods: HashMap<String, CallableObject>,
}

pub fn create_instance(class: Rc<LoxClass>) -> Rc<RefCell<LoxInstance>> {
    let properties: HashMap<String, Object> = HashMap::new(); // TODO get the properties set in the "init" function
    Rc::new(RefCell::new(LoxInstance {
        properties,
        class: Rc::downgrade(&class),
    }))
}

impl Debug for LoxClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&self.name).finish()
    }
}
//////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////LoxInstance///////////////////////////////////////////////////

#[derive(Clone)]
pub struct LoxInstance {
    pub properties: HashMap<String, Object>,
    pub class: Weak<LoxClass>,
}

impl Debug for LoxInstance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("").finish()
    }
}
/////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////CallableObject/////////////////////////////////////////
#[derive(Clone)]
pub struct CallableObject {
    pub name: String,
    pub parameters: Vec<String>,
    pub function_block: Box<Statement>,
    pub environment: Rc<RefCell<EnvironmentNode>>,
}

impl CallableObject {
    pub fn new(
        name: &String,
        parameters: &Vec<String>,
        function_block: &Statement,
        parent_environment: &Weak<RefCell<EnvironmentNode>>,
    ) -> Self {
        let mut env = EnvironmentNode::new();
        env.parent = Some(parent_environment.clone());
        CallableObject {
            name: name.clone(),
            parameters: parameters.clone(),
            function_block: Box::new(function_block.clone()),
            environment: Rc::new(RefCell::new(env)),
        }
    }
}

impl Debug for CallableObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple(&self.name)
            .field(&self.parameters)
            .field(&self.function_block)
            .finish()
    }
}
//////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////Object////////////////////////////////////////////
#[derive(Clone, Debug)]
pub enum Object {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Callable(CallableObject),
    Class(Rc<LoxClass>),
    Instance(Rc<RefCell<LoxInstance>>),
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
            Object::String(string) => {
                write!(f, "{}", string)
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
                write!(f, "fn <{}>", &callable.name)
                // TODO expand a little bit more
            }
            Object::Class(class_object) => {
                write!(f, "class<{}>", (*class_object).name)
            }
            Object::Instance(instance) => {
                let instance = instance.borrow();
                let class = instance.class.upgrade().unwrap();
                write!(f, "class<{}>instance", class.name)?;
                for (prop, value) in &instance.properties {
                    write!(f, "\n \"{}\" : {}", prop, value)?;
                }
                Ok(())
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
//////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////Environment///////////////////////////////////////////
pub struct Environment {
    pub variables: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            variables: HashMap::new(),
        }
    }

    pub fn lookup(&self, name: &String) -> Option<&Object> {
        let lookup_result = self.variables.get(name);
        if let Some(value) = lookup_result {
            return Some(&value);
        }
        return None;
    }

    pub fn update_or_add(&mut self, name: &str, value: Object) {
        let lookup_result = self.variables.get_mut(name);
        if let Some(object) = lookup_result {
            *object = value;
        } else {
            self.variables.insert(name.to_string(), value);
        }
    }
}
//////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////EnvironmentNode//////////////////////////////////
pub struct EnvironmentNode {
    pub environment: Environment,
    pub parent: Option<Weak<RefCell<EnvironmentNode>>>,
    pub children: Vec<Rc<RefCell<EnvironmentNode>>>,
}

impl EnvironmentNode {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
            parent: None,
            children: Vec::new(),
        }
    }
}

pub fn add_child(parent: &mut Rc<RefCell<EnvironmentNode>>) -> Weak<RefCell<EnvironmentNode>> {
    let child = Rc::new(RefCell::new(EnvironmentNode::new()));
    (*child).borrow_mut().parent = Some(Rc::downgrade(parent));
    parent.as_ref().borrow_mut().children.push(child.clone());
    return Rc::downgrade(&child);
}
//////////////////////////////////////////////////////////////////////////////////////////////
