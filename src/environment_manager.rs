use crate::parser::{LiteralType, Statement};

#[derive(Clone)]
pub enum Object {
    Number(f64),
    String(Vec<u8>),
    True,
    False,
    Nil,
    Callable(Vec<Vec<u8>>, Box<Statement>, usize),
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
            Object::Callable(_, _, _) => {
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

pub struct Environment {
    variables: Vec<NamedObject>,
}

impl Environment {
    pub fn new() -> Self {
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

    pub fn lookup_global(&self, name: &Vec<u8>) -> Option<&Object> {
        if let Some(name_value_pair) = self
            .variables
            .iter()
            .find(|x| Self::name_comp(&(*x).name, name))
        {
            return Some(&name_value_pair.object);
        }
        return None;
    }

    pub fn update_or_add(&mut self, name: &Vec<u8>, value: Object) {
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
    pub fn update(&mut self, name: &Vec<u8>, value: &Object) -> bool {
        if let Some(name_value_pair) = self
            .variables
            .iter_mut()
            .find(|x| Self::name_comp(&(*x).name, &name))
        {
            name_value_pair.object = value.clone();
            return true;
        } else {
            return false;
        }
    }
}
