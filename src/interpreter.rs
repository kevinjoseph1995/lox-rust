use crate::error::LoxError;
use crate::parser::{
    BinaryOperator, Expression, LiteralType, LogicalOperator, Program, Statement, UnaryOperator,
};

struct Environment {
    variables: Vec<(Vec<u8>, LiteralType)>,
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

    fn lookup_global(&self, name: &Vec<u8>) -> Option<&LiteralType> {
        if let Some(name_value_pair) = self
            .variables
            .iter()
            .find(|x| Self::name_comp(&(*x).0, name))
        {
            return Some(&name_value_pair.1);
        }
        return None;
    }

    fn update_or_add(&mut self, name: &Vec<u8>, value: LiteralType) {
        if let Some(name_value_pair) = self
            .variables
            .iter_mut()
            .find(|x| Self::name_comp(&(*x).0, &name))
        {
            name_value_pair.1 = value
        } else {
            self.variables.push((name.clone(), value));
        }
    }
    fn update(&mut self, name: &Vec<u8>, value: &LiteralType) -> bool {
        if let Some(name_value_pair) = self
            .variables
            .iter_mut()
            .find(|x| Self::name_comp(&(*x).0, &name))
        {
            name_value_pair.1 = value.clone();
            return true;
        } else {
            return false;
        }
    }
}

fn is_true_value(value: &LiteralType) -> bool {
    match value {
        LiteralType::True => true,
        LiteralType::Number(num) => {
            if num.clone() != 0.0f64 {
                return true;
            }
            false
        }
        LiteralType::String(_) => true,
        _ => false,
    }
}

pub struct Interpreter {
    environment_stack: Vec<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment_stack: vec![Environment::new()], // Push the global environment onto the stack
        }
    }

    pub fn lookup_variable(&self, name: &Vec<u8>) -> Option<&LiteralType> {
        for env in self.environment_stack.iter().rev() {
            if let Some(value) = env.lookup_global(name) {
                return Some(value);
            }
        }
        None
    }

    fn update(&mut self, name: &Vec<u8>, value: LiteralType) -> bool {
        for env in self.environment_stack.iter_mut().rev() {
            if env.update(name, &value) {
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
                for statement in block_statements {
                    self.handle_statement(statement)?;
                }
                self.environment_stack.pop();
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
        }
        Ok(())
    }

    fn evaluate(&mut self, expression: &Expression) -> Result<LiteralType, LoxError> {
        match expression {
            Expression::Literal(literal_type) => Ok(literal_type.clone()),
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
                    return Ok(LiteralType::Nil);
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
                            return Ok(LiteralType::False);
                        }
                        let rhs_value = self.evaluate(rhs)?;
                        if is_true_value(&rhs_value) {
                            return Ok(LiteralType::True);
                        }
                        return Ok(LiteralType::False);
                    }
                    LogicalOperator::Or => {
                        if is_true_value(&lhs_value) {
                            return Ok(LiteralType::True);
                        }
                        let rhs_value = self.evaluate(rhs)?;
                        if is_true_value(&rhs_value) {
                            return Ok(LiteralType::True);
                        }
                        return Ok(LiteralType::False);
                    }
                }
            }
        }
    }
}
