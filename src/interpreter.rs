use crate::error::LoxError;
use crate::parser::{
    BinaryOperator, Expression, LiteralType, LogicalOperator, Program, Statement, UnaryOperator,
};

#[derive(Clone)]
enum Object {
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

    fn lookup_global(&self, name: &Vec<u8>) -> Option<&Object> {
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
    fn update(&mut self, name: &Vec<u8>, value: &Object) -> bool {
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

pub struct Interpreter {
    environment_stack: Vec<Environment>,
    current_environment_index: usize,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment_stack: vec![Environment::new()], // Push the global environment onto the stack
            current_environment_index: 0, // Moke the current environment point to the global environment
        }
    }

    fn lookup_variable(&self, name: &Vec<u8>) -> Option<&Object> {
        for index in (0..=self.current_environment_index).rev() {
            if let Some(value) = self.environment_stack[index].lookup_global(name) {
                return Some(value);
            }
        }
        None
    }

    fn update(&mut self, name: &Vec<u8>, value: Object) -> bool {
        for index in (0..=self.current_environment_index).rev() {
            if self.environment_stack[index].update(name, &value) {
                return true;
            }
        }
        return false;
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
                println!("{}", result)
            }
            Statement::VariableDeclaration(name, expression) => {
                let value = self.evaluate(expression)?;
                match self.environment_stack.last_mut() {
                    Some(environment) => {
                        environment.update_or_add(name, value);
                    }
                    None => {
                        return Err(LoxError::InternalError(
                            "Empty environment stack, something really bad happened".to_string(),
                        ))
                    }
                }
            }
            Statement::Block(block_statements) => {
                // Entered new scope
                self.environment_stack.push(Environment::new());
                self.current_environment_index = self.environment_stack.len() - 1;
                for statement in block_statements {
                    self.handle_statement(statement)?;
                }
                self.environment_stack.pop();
                self.current_environment_index = self.environment_stack.len() - 1;
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
            Statement::FunctionDeclaration(_name, parameters, _body) => {
                todo!();
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
                        Object::Callable(_, _, _) => Err(LoxError::RuntimeError(
                            "Cannot negate callable object".to_string(),
                        )),
                    },
                    UnaryOperator::Not => match value {
                        Object::String(_) => Ok(Object::False),
                        Object::Number(_) => Ok(Object::False),
                        Object::True => Ok(Object::False),
                        Object::False => Ok(Object::True),
                        Object::Nil => Ok(Object::True),
                        Object::Callable(_, _, _) => Err(LoxError::RuntimeError(
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
                            let mut v3 = Vec::new();
                            v3.extend_from_slice(&str1[..]);
                            v3.extend_from_slice(&str2[..]);
                            Ok(Object::String(v3))
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
                if let Some(value) = self.lookup_variable(&name) {
                    return Ok(value.clone()); // Find a better way to do lookup without copying
                } else {
                    let name_str = std::str::from_utf8(&name).unwrap();
                    return Err(LoxError::RuntimeError(format!(
                        "Use of undeclared variable {}",
                        name_str
                    )));
                }
            }
            Expression::Assignment(name, expr) => {
                let new_value = self.evaluate(expr)?;
                if self.update(name, new_value) {
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
            Expression::Call(_callee, _arguments) => {
                todo!()
            }
        }
    }
}
