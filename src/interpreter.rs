use crate::environment_manager::{is_true_value, EnvironmentManager, Object};

use crate::error::LoxError;
use crate::parser::{
    BinaryOperator, Expression, LogicalOperator, Program, Statement, UnaryOperator,
};

pub struct Interpreter {
    environment_manager: EnvironmentManager,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment_manager: EnvironmentManager::new(),
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
                println!("{}", result)
            }
            Statement::VariableDeclaration(name, expression) => {
                let value = self.evaluate(expression)?;
                self.environment_manager.update_or_add(name, value);
            }
            Statement::Block(block_statements) => {
                // Entered new scope
                self.environment_manager.push_context();
                for statement in block_statements {
                    self.handle_statement(statement)?;
                }
                self.environment_manager.pop_context();
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
            Statement::FunctionDeclaration(_name, _parameters, _body) => {
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
                        Object::Callable(_, _) => Err(LoxError::RuntimeError(
                            "Cannot negate callable object".to_string(),
                        )),
                    },
                    UnaryOperator::Not => match value {
                        Object::String(_) => Ok(Object::False),
                        Object::Number(_) => Ok(Object::False),
                        Object::True => Ok(Object::False),
                        Object::False => Ok(Object::True),
                        Object::Nil => Ok(Object::True),
                        Object::Callable(_, _) => Err(LoxError::RuntimeError(
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
                if let Some(value) = self.environment_manager.lookup(&name) {
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
                if self.environment_manager.update(&name, &new_value) {
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
