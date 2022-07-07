use std::cell::RefCell;
use std::fmt::Debug;
use std::fmt::Formatter;
use std::rc::{Rc, Weak};

use crate::error::LoxError;
use crate::parser::{
    BinaryOperator, Expression, LiteralType, LogicalOperator, Program, Statement, UnaryOperator,
};

pub struct Interpreter {
    global_environment: Rc<RefCell<EnvironmentNode>>, // Root of our environment tree. Keeps all the other nodes alive
    current: Weak<RefCell<EnvironmentNode>>, // Points to the current environment based on what state the interpreter's is in
}

impl Interpreter {
    pub fn new() -> Self {
        let global_environment = Rc::new(RefCell::new(EnvironmentNode::new()));
        let weak_ptr_to_global = Rc::downgrade(&global_environment);
        Interpreter {
            global_environment,
            current: weak_ptr_to_global,
        }
    }

    pub fn interpret(&mut self, program: Program) -> Result<(), LoxError> {
        let mut statements = program.statements;
        for statement in &mut statements {
            self.handle_statement(statement)?;
        }
        Ok(())
    }

    pub fn handle_statement(&mut self, statement: &Statement) -> Result<(), LoxError> {
        match statement {
            Statement::Expression(expression) => {
                self.evaluate(expression)?;
            }
            Statement::Print(expression) => {
                let result = self.evaluate(expression)?;
                print!("{}", result);
            }
            Statement::VariableDeclaration(name, expression) => {
                let value = self.evaluate(expression)?;
                self.update_or_add(name, value);
            }
            Statement::Block(block_statements) => {
                // Entered new scope
                let prev = self.current.clone();
                self.current = add_child(&mut self.current.upgrade().unwrap());
                for statement in block_statements {
                    self.handle_statement(statement)?;
                }
                self.current = prev;
            }
            Statement::If(condition, then_clause, else_clause) => {
                let condition_value = self.evaluate(condition)?;
                if is_true_value(&condition_value) {
                    self.handle_statement(then_clause)?;
                } else {
                    if let Some(else_clause) = else_clause {
                        self.handle_statement(else_clause)?;
                    }
                }
            }
            Statement::While(condition_expr, statement) => {
                let mut condition_value = self.evaluate(condition_expr)?;
                while is_true_value(&condition_value) {
                    self.handle_statement(statement)?;
                    condition_value = self.evaluate(condition_expr)?;
                }
            }
            Statement::FunctionDeclaration(name, parameters, body) => {
                self.add_callable_object(name, parameters, body);
            }
            Statement::Return(exp_opt) => {
                if let Some(expression) = exp_opt {
                    let value = self.evaluate(expression)?;
                    return Err(LoxError::Return(value));
                } else {
                    return Err(LoxError::Return(Object::Nil));
                }
            }
            Statement::Println(expression) => {
                let result = self.evaluate(expression)?;
                println!("{}", result)
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expression: &Expression) -> Result<Object, LoxError> {
        match expression {
            Expression::Literal(literal_type) => Ok(Object::from(literal_type.clone())),
            Expression::Grouping(grouped_expression) => {
                let value = self.evaluate(grouped_expression)?;
                Ok(value)
            }
            Expression::Unary(unary_operator, unary_operator_expression) => {
                let value = self.evaluate(unary_operator_expression)?;
                match unary_operator {
                    UnaryOperator::Negate => match value {
                        Object::String(_) => {
                            return Err(LoxError::RuntimeError(
                                "Invalid runtime type for negation operation".to_string(),
                            ));
                        }
                        Object::Number(number) => Ok(Object::Number(-number)),
                        Object::True => Ok(Object::Number(-1.0f64)), // Implicit cast to number
                        Object::False => Ok(Object::Number(0.0f64)), // Implicit cast to number
                        Object::Nil => Ok(Object::Nil),
                        Object::Callable(_) => Err(LoxError::RuntimeError(
                            "Cannot negate callable object".to_string(),
                        )),
                    },
                    UnaryOperator::Not => match value {
                        Object::String(_) => Ok(Object::False),
                        Object::Number(_) => Ok(Object::False),
                        Object::True => Ok(Object::False),
                        Object::False => Ok(Object::True),
                        Object::Nil => Ok(Object::True),
                        Object::Callable(_) => Err(LoxError::RuntimeError(
                            "Cannot call logical not on callable object".to_string(),
                        )),
                    },
                }
            }
            Expression::Binary(lhs_expression, binary_operator, rhs_expression) => {
                let left_value = self.evaluate(lhs_expression)?;
                let right_value = self.evaluate(rhs_expression)?;
                match binary_operator {
                    BinaryOperator::Equal => Ok(Object::from(left_value == right_value)),
                    BinaryOperator::NotEqual => Ok(Object::from(left_value != right_value)),
                    BinaryOperator::Plus => match (left_value, right_value) {
                        (Object::String(str1), Object::String(str2)) => {
                            Ok(Object::String(str1.clone() + &str2))
                        }
                        (Object::Number(num1), Object::Number(num2)) => {
                            Ok(Object::Number(num1 + num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands for +
                                operator"
                                    .to_string(),
                            ))
                        }
                    },
                    // Only for number, true and false type
                    BinaryOperator::LessThan => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            if num1 < num2 {
                                Ok(Object::True)
                            } else {
                                Ok(Object::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to < operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::LessThanEqual => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            if num1 <= num2 {
                                Ok(Object::True)
                            } else {
                                Ok(Object::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to <= operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::GreaterThan => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            if num1 > num2 {
                                Ok(Object::True)
                            } else {
                                Ok(Object::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to > operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::GreaterThanEqual => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            if num1 >= num2 {
                                Ok(Object::True)
                            } else {
                                Ok(Object::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to >= operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::Minus => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            Ok(Object::Number(num1 - num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to - operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::Multiply => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            Ok(Object::Number(num1 * num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to *  operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::Divide => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            Ok(Object::Number(num1 / num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to / operator".to_string(),
                            ))
                        }
                    },
                }
            }
            Expression::Identifier(name) => {
                if let Some(value) = self.lookup(&name) {
                    return Ok(value.clone()); // Find a better way to do lookup without copying
                } else {
                    return Err(LoxError::RuntimeError(format!(
                        "Use of undeclared variable {}",
                        name
                    )));
                }
            }
            Expression::Assignment(name, expr) => {
                let new_value = self.evaluate(expr)?;
                if self.update(&name, &new_value) {
                    return Ok(Object::Nil);
                } else {
                    return Err(LoxError::RuntimeError(format!(
                        "Reference to undeclared variable"
                    )));
                }
            }
            Expression::Logical(lhs, op, rhs) => {
                let lhs_value = self.evaluate(lhs)?;
                match op {
                    LogicalOperator::And => {
                        if !is_true_value(&lhs_value) {
                            return Ok(Object::False);
                        }
                        let rhs_value = self.evaluate(rhs)?;
                        if is_true_value(&rhs_value) {
                            return Ok(Object::True);
                        }
                        return Ok(Object::False);
                    }
                    LogicalOperator::Or => {
                        if is_true_value(&lhs_value) {
                            return Ok(Object::True);
                        }
                        let rhs_value = self.evaluate(rhs)?;
                        if is_true_value(&rhs_value) {
                            return Ok(Object::True);
                        }
                        return Ok(Object::False);
                    }
                }
            }
            Expression::Call(call_expression, arguments) => {
                let mut callable = self.evaluate(call_expression)?;
                match &mut callable {
                    Object::Callable(callable_object) => {
                        if callable_object.parameters.len() != arguments.len() {
                            // Check arity
                            return Err(LoxError::RuntimeError(format!(
                                "{} expects {} argument/s, found {}",
                                &callable_object.name,
                                callable_object.parameters.len(),
                                arguments.len()
                            )));
                        }
                        let mut arg_values = Vec::new();
                        for arg_expr in arguments {
                            let value = self.evaluate(arg_expr)?;
                            arg_values.push(value);
                        }
                        let prev = self.current.clone();
                        self.current =
                            add_child(&mut callable_object.parent_environment.upgrade().unwrap());
                        for (param, arg_value) in callable_object.parameters.iter().zip(arg_values)
                        {
                            self.update_or_add(param, arg_value);
                        }
                        let return_value: Result<Object, LoxError>;
                        match self.handle_statement(&callable_object.function_block) {
                            Ok(_) => return_value = Ok(Object::Nil),
                            Err(err) => match err {
                                LoxError::Return(value) => {
                                    return_value = Ok(value);
                                }
                                _ => return Err(err),
                            },
                        }
                        self.current = prev;
                        return return_value;
                    }
                    _ => {
                        return Err(LoxError::RuntimeError(format!(
                            "{} is not a callable type",
                            callable
                        )))
                    }
                }
            }
        }
    }
    fn print_environment_node_helper(&self, node: Weak<RefCell<EnvironmentNode>>, indent: usize) {
        for var in &node.upgrade().unwrap().borrow().environment.variables {
            print!("{:<width$}", "", width = indent + 2);
            println!("name: {} value={}", &var.name, var.object);
        }
    }

    fn print_environment_tree_helper(
        &mut self,
        node: Weak<RefCell<EnvironmentNode>>,
        indent: usize,
    ) {
        print!("{:<width$}", "", width = indent);
        println!("{{");
        self.print_environment_node_helper(node.clone(), indent);
        for child in &node.upgrade().unwrap().borrow().children {
            self.print_environment_tree_helper(Rc::downgrade(&child), indent + 4);
        }
        print!("{:<width$}", "", width = indent);
        println!("}}");
    }
    #[allow(dead_code)]
    fn print_environment_tree(&mut self) {
        self.print_environment_tree_helper(Rc::downgrade(&self.global_environment), 0);
    }
    #[allow(dead_code)]
    fn print_current_environment_node(&mut self) {
        self.print_environment_tree_helper(self.current.clone(), 0);
    }

    fn lookup(&self, name: &String) -> Option<Object> {
        let mut current = self.current.clone();
        loop {
            let rc = current.upgrade().unwrap();
            let current_ref = rc.borrow();
            if let Some(value) = current_ref.environment.lookup(name) {
                return Some(value.clone());
            }
            let parent;

            match &current_ref.parent {
                Some(p) => parent = p.clone(),
                None => break,
            }
            current = parent;
        }
        return None;
    }
    fn update(&mut self, name: &String, value: &Object) -> bool {
        let mut current = self.current.clone();
        loop {
            let rc = current.upgrade().unwrap();
            let mut current_mut_ref = rc.borrow_mut();

            if current_mut_ref.environment.lookup(name).is_some() {
                current_mut_ref
                    .environment
                    .update_or_add(name, value.clone());
                return true;
            }

            let parent;
            match &current_mut_ref.parent {
                Some(p) => parent = p.clone(),
                None => break,
            }
            current = parent;
        }
        return false;
    }

    fn update_or_add(&mut self, name: &String, value: Object) {
        self.current
            .upgrade()
            .unwrap()
            .as_ref()
            .borrow_mut()
            .environment
            .update_or_add(name, value);
    }

    fn add_callable_object(
        &mut self,
        name: &String,
        parameters: &Vec<String>,
        body: &Box<Statement>,
    ) {
        let callable = CallableObject {
            name: name.clone(),
            parameters: parameters.clone(),
            function_block: body.clone(),
            parent_environment: self.current.clone(),
        };
        self.update_or_add(name, Object::Callable(callable));
    }
}

#[derive(Clone, Debug)]
pub enum Object {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
    Callable(CallableObject),
}

#[derive(Clone)]
pub struct CallableObject {
    name: String,
    parameters: Vec<String>,
    function_block: Box<Statement>,
    parent_environment: Weak<RefCell<EnvironmentNode>>,
}

impl Debug for CallableObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("")
            .field(&self.name)
            .field(&self.parameters)
            .field(&self.function_block)
            .finish()
    }
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
        }
    }
}

fn is_true_value(value: &Object) -> bool {
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
    name: String,
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

    fn lookup(&self, name: &String) -> Option<&Object> {
        if let Some(name_value_pair) = self.variables.iter().find(|x| &(*x).name == name) {
            return Some(&name_value_pair.object);
        }
        return None;
    }

    fn update_or_add(&mut self, name: &String, value: Object) {
        if let Some(name_value_pair) = self.variables.iter_mut().find(|x| &(*x).name == name) {
            name_value_pair.object = value
        } else {
            self.variables.push(NamedObject {
                name: name.clone(),
                object: value,
            });
        }
    }
}

struct EnvironmentNode {
    environment: Environment,
    parent: Option<Weak<RefCell<EnvironmentNode>>>,
    children: Vec<Rc<RefCell<EnvironmentNode>>>,
}

impl EnvironmentNode {
    fn new() -> Self {
        Self {
            environment: Environment::new(),
            parent: None,
            children: Vec::new(),
        }
    }
}

fn add_child(parent: &mut Rc<RefCell<EnvironmentNode>>) -> Weak<RefCell<EnvironmentNode>> {
    let child = Rc::new(RefCell::new(EnvironmentNode::new()));
    (*child).borrow_mut().parent = Some(Rc::downgrade(parent));
    parent.as_ref().borrow_mut().children.push(child.clone());
    return Rc::downgrade(&child);
}
