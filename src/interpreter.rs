use crate::error::LoxError;
use crate::parser::{BinaryOperator, Expression, LiteralType, UnaryOperator};

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

impl Interpreter {
    pub fn interpret(&mut self, expression: &Box<Expression>) -> Result<(), LoxError> {
        !todo!();
    }
    fn evaluate(&mut self, expression: &Box<Expression>) -> Result<DynamicType, LoxError> {
        match expression.as_ref() {
            Expression::Literal(literal_type) => Ok(DynamicType::from(literal_type)),
            Expression::Grouping(_) => todo!(),
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
                        DynamicType::Number(number) => Ok(DynamicType::False),
                        DynamicType::True => Ok(DynamicType::False),
                        DynamicType::False => Ok(DynamicType::True),
                        DynamicType::Nil => Ok(DynamicType::True),
                    },
                }
            }
            Expression::Binary(lhs_expression, binary_operator, rhs_expression) => {
                let left_value = self.evaluate(lhs_expression)?;
                let right_value = self.evaluate(lhs_expression)?;
                match binary_operator {
                    BinaryOperator::Equal => Ok(DynamicType::from(left_value == right_value)),
                    BinaryOperator::NotEqual => Ok(DynamicType::from(left_value != right_value)),
                    BinaryOperator::Plus => todo!(),
                    // Only for number, true and false type
                    BinaryOperator::LessThan => todo!(),
                    BinaryOperator::LessThanEqual => todo!(),
                    BinaryOperator::GreaterThan => todo!(),
                    BinaryOperator::GreaterThanEqual => todo!(),
                    BinaryOperator::Minus => todo!(),
                    BinaryOperator::Multiply => todo!(),
                    BinaryOperator::Divide => todo!(),
                }
            }
        }
    }
}
