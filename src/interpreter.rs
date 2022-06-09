use crate::error::LoxError;
use crate::parser::{BinaryOperator, Expression, LiteralType, Program, Statement, UnaryOperator};

pub struct Interpreter {
    // Currently stateless, add state here
}

enum DynamicType {
    Number(f64),
    String(Vec<u8>),
    True,
    False,
    Nil,
}

impl Eq for DynamicType {}

impl PartialEq for DynamicType {
    fn eq(&self, other: &Self) -> bool {
        if let (DynamicType::Number(value1), DynamicType::Number(value2)) = (&self, &other) {
            return value1 == value2;
        }
        if let (DynamicType::String(value1), DynamicType::String(value2)) = (&self, &other) {
            return value1 == value2;
        }
        return self == other;
    }
}

impl From<bool> for DynamicType {
    fn from(v: bool) -> DynamicType {
        if v {
            DynamicType::True
        } else {
            DynamicType::False
        }
    }
}

impl From<&LiteralType> for DynamicType {
    fn from(x: &LiteralType) -> Self {
        match x {
            LiteralType::Number(number) => Self::Number(number.clone()),
            LiteralType::String(string) => Self::String(string.clone()),
            LiteralType::True => Self::True,
            LiteralType::False => Self::False,
            LiteralType::Nil => Self::Nil,
        }
    }
}

impl std::fmt::Display for DynamicType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DynamicType::Number(number) => {
                write!(f, "{}", number)
            }
            DynamicType::String(u8_vec) => {
                let result = std::str::from_utf8(u8_vec);

                match result {
                    Ok(str_val) => write!(f, "{}", str_val),
                    Err(_) => write!(f, "UTF decoding error"),
                }
            }
            DynamicType::True => {
                write!(f, "True")
            }
            DynamicType::False => {
                write!(f, "False")
            }
            DynamicType::Nil => {
                write!(f, "Nil")
            }
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, program: &Program) -> Result<(), LoxError> {
        for statement in &program.statements {
            match statement {
                Statement::Expression(expression) => !todo!(),
                Statement::Print(expression) => {
                    let result = self.evaluate(&expression)?;
                    println!("{}", result)
                }
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expression: &Box<Expression>) -> Result<DynamicType, LoxError> {
        match expression.as_ref() {
            Expression::Literal(literal_type) => Ok(DynamicType::from(literal_type)),
            Expression::Grouping(grouped_expression) => {
                let value = self.evaluate(grouped_expression)?;
                Ok(value)
            }
            Expression::Unary(unary_operator, unary_operator_expression) => {
                let value = self.evaluate(unary_operator_expression)?;
                match unary_operator {
                    UnaryOperator::Negate => match value {
                        DynamicType::String(_) => {
                            return Err(LoxError::RuntimeError(
                                "Invalid runtime type for negation operation".to_string(),
                            ));
                        }
                        DynamicType::Number(number) => Ok(DynamicType::Number(-number)),
                        DynamicType::True => Ok(DynamicType::Number(-1.0f64)), // Implicit cast to number
                        DynamicType::False => Ok(DynamicType::Number(0.0f64)), // Implicit cast to number
                        DynamicType::Nil => Ok(DynamicType::Nil),
                    },
                    UnaryOperator::Not => match value {
                        DynamicType::String(_) => Ok(DynamicType::False),
                        DynamicType::Number(_) => Ok(DynamicType::False),
                        DynamicType::True => Ok(DynamicType::False),
                        DynamicType::False => Ok(DynamicType::True),
                        DynamicType::Nil => Ok(DynamicType::True),
                    },
                }
            }
            Expression::Binary(lhs_expression, binary_operator, rhs_expression) => {
                let left_value = self.evaluate(lhs_expression)?;
                let right_value = self.evaluate(rhs_expression)?;
                match binary_operator {
                    BinaryOperator::Equal => Ok(DynamicType::from(left_value == right_value)),
                    BinaryOperator::NotEqual => Ok(DynamicType::from(left_value != right_value)),
                    BinaryOperator::Plus => match (left_value, right_value) {
                        (DynamicType::String(str1), DynamicType::String(str2)) => {
                            let mut v3 = Vec::new();
                            v3.extend_from_slice(&str1[..]);
                            v3.extend_from_slice(&str2[..]);
                            Ok(DynamicType::String(v3))
                        }
                        (DynamicType::Number(num1), DynamicType::Number(num2)) => {
                            Ok(DynamicType::Number(num1 + num2))
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
                        (DynamicType::Number(num1), DynamicType::Number(num2)) => {
                            if num1 < num2 {
                                Ok(DynamicType::True)
                            } else {
                                Ok(DynamicType::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to < operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::LessThanEqual => match (left_value, right_value) {
                        (DynamicType::Number(num1), DynamicType::Number(num2)) => {
                            if num1 <= num2 {
                                Ok(DynamicType::True)
                            } else {
                                Ok(DynamicType::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to <= operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::GreaterThan => match (left_value, right_value) {
                        (DynamicType::Number(num1), DynamicType::Number(num2)) => {
                            if num1 > num2 {
                                Ok(DynamicType::True)
                            } else {
                                Ok(DynamicType::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to > operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::GreaterThanEqual => match (left_value, right_value) {
                        (DynamicType::Number(num1), DynamicType::Number(num2)) => {
                            if num1 >= num2 {
                                Ok(DynamicType::True)
                            } else {
                                Ok(DynamicType::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to >= operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::Minus => match (left_value, right_value) {
                        (DynamicType::Number(num1), DynamicType::Number(num2)) => {
                            Ok(DynamicType::Number(num1 - num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to - operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::Multiply => match (left_value, right_value) {
                        (DynamicType::Number(num1), DynamicType::Number(num2)) => {
                            Ok(DynamicType::Number(num1 * num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to *  operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::Divide => match (left_value, right_value) {
                        (DynamicType::Number(num1), DynamicType::Number(num2)) => {
                            Ok(DynamicType::Number(num1 / num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to / operator".to_string(),
                            ))
                        }
                    },
                }
            }
        }
    }
}
