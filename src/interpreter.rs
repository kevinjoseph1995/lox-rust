use crate::error::LoxError;
use crate::parser::{BinaryOperator, Expression, LiteralType, Program, Statement, UnaryOperator};

struct Environment {
    global_variables: Vec<(Vec<u8>, LiteralType)>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            global_variables: Vec::new(),
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

    fn lookup_global(&self, name: &Vec<u8>) -> Option<&LiteralType> {
        if let Some(name_value_pair) = self
            .global_variables
            .iter()
            .find(|x| Self::name_comp(&(*x).0, name))
        {
            return Some(&name_value_pair.1);
        }
        return None;
    }

    fn update_global(&mut self, name: &Vec<u8>, value: LiteralType) {
        if let Some(name_value_pair) = self
            .global_variables
            .iter_mut()
            .find(|x| Self::name_comp(&(*x).0, &name))
        {
            name_value_pair.1 = value
        } else {
            self.global_variables.push((name.clone(), value));
        }
    }
}

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, program: Program) -> Result<(), LoxError> {
        let mut statements = program.statements;
        for statement in &mut statements {
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
                    self.environment.update_global(&name, value);
                }
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expression: &mut Box<Expression>) -> Result<LiteralType, LoxError> {
        match expression.as_mut() {
            Expression::Literal(literal_type) => Ok(literal_type.take()),
            Expression::Grouping(grouped_expression) => {
                let value = self.evaluate(grouped_expression)?;
                Ok(value)
            }
            Expression::Unary(unary_operator, unary_operator_expression) => {
                let value = self.evaluate(unary_operator_expression)?;
                match unary_operator {
                    UnaryOperator::Negate => match value {
                        LiteralType::String(_) => {
                            return Err(LoxError::RuntimeError(
                                "Invalid runtime type for negation operation".to_string(),
                            ));
                        }
                        LiteralType::Number(number) => Ok(LiteralType::Number(-number)),
                        LiteralType::True => Ok(LiteralType::Number(-1.0f64)), // Implicit cast to number
                        LiteralType::False => Ok(LiteralType::Number(0.0f64)), // Implicit cast to number
                        LiteralType::Nil => Ok(LiteralType::Nil),
                    },
                    UnaryOperator::Not => match value {
                        LiteralType::String(_) => Ok(LiteralType::False),
                        LiteralType::Number(_) => Ok(LiteralType::False),
                        LiteralType::True => Ok(LiteralType::False),
                        LiteralType::False => Ok(LiteralType::True),
                        LiteralType::Nil => Ok(LiteralType::True),
                    },
                }
            }
            Expression::Binary(lhs_expression, binary_operator, rhs_expression) => {
                let left_value = self.evaluate(lhs_expression)?;
                let right_value = self.evaluate(rhs_expression)?;
                match binary_operator {
                    BinaryOperator::Equal => Ok(LiteralType::from(left_value == right_value)),
                    BinaryOperator::NotEqual => Ok(LiteralType::from(left_value != right_value)),
                    BinaryOperator::Plus => match (left_value, right_value) {
                        (LiteralType::String(str1), LiteralType::String(str2)) => {
                            let mut v3 = Vec::new();
                            v3.extend_from_slice(&str1[..]);
                            v3.extend_from_slice(&str2[..]);
                            Ok(LiteralType::String(v3))
                        }
                        (LiteralType::Number(num1), LiteralType::Number(num2)) => {
                            Ok(LiteralType::Number(num1 + num2))
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
                        (LiteralType::Number(num1), LiteralType::Number(num2)) => {
                            if num1 < num2 {
                                Ok(LiteralType::True)
                            } else {
                                Ok(LiteralType::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to < operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::LessThanEqual => match (left_value, right_value) {
                        (LiteralType::Number(num1), LiteralType::Number(num2)) => {
                            if num1 <= num2 {
                                Ok(LiteralType::True)
                            } else {
                                Ok(LiteralType::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to <= operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::GreaterThan => match (left_value, right_value) {
                        (LiteralType::Number(num1), LiteralType::Number(num2)) => {
                            if num1 > num2 {
                                Ok(LiteralType::True)
                            } else {
                                Ok(LiteralType::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to > operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::GreaterThanEqual => match (left_value, right_value) {
                        (LiteralType::Number(num1), LiteralType::Number(num2)) => {
                            if num1 >= num2 {
                                Ok(LiteralType::True)
                            } else {
                                Ok(LiteralType::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to >= operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::Minus => match (left_value, right_value) {
                        (LiteralType::Number(num1), LiteralType::Number(num2)) => {
                            Ok(LiteralType::Number(num1 - num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to - operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::Multiply => match (left_value, right_value) {
                        (LiteralType::Number(num1), LiteralType::Number(num2)) => {
                            Ok(LiteralType::Number(num1 * num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to *  operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::Divide => match (left_value, right_value) {
                        (LiteralType::Number(num1), LiteralType::Number(num2)) => {
                            Ok(LiteralType::Number(num1 / num2))
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
                if let Some(value) = self.environment.lookup_global(&name) {
                    return Ok(value.clone()); // Find a better way to do lookup without copying
                } else {
                    let name_str = std::str::from_utf8(&name).unwrap();
                    return Err(LoxError::RuntimeError(format!(
                        "Could not find {} in global namespace",
                        name_str
                    )));
                }
            }
            Expression::Assignment(name, expr) => {
                if let Some(_) = self.environment.lookup_global(&name) {
                    let new_value = self.evaluate(expr)?;
                    self.environment.update_global(name, new_value);
                    return Ok(LiteralType::Nil);
                } else {
                    return Err(LoxError::RuntimeError(format!(
                        "Reference to undeclared variable"
                    )));
                }
            }
        }
    }
}
