use crate::{
    error::LoxError,
    tokens::{Token, TokenType},
};

pub enum Expression {
    Literal(LiteralType),
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Grouping(Box<Expression>),
}

pub enum UnaryOperator {
    Negate,
    Not,
}

pub enum BinaryOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
}

pub enum LiteralType {
    Number(f64),
    String(Vec<u8>),
    True,
    False,
    Nil,
}

/*
Expression grammar for Lox from https://craftinginterpreters.com/parsing-expressions.html
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
*/

pub struct Parser<'a> {
    tokens: &'a [Token], // The tokens are owned by the scanner
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn new<'b: 'a>(tokens: &'b [Token]) -> Self {
        Parser { tokens, index: 0 }
    }

    fn expression(&mut self) -> Result<Box<Expression>, LoxError> {
        !todo!();
    }

    fn unary(&mut self) -> Result<Box<Expression>, LoxError> {
        match &self.tokens[self.index].token_type {
            TokenType::Minus => {
                self.index += 1;
                let right_expression = self.unary()?;
                Ok(Box::new(Expression::Unary(
                    UnaryOperator::Negate,
                    right_expression,
                )))
            }
            TokenType::Bang => {
                self.index += 1;
                let right_expression = self.unary()?;
                Ok(Box::new(Expression::Unary(
                    UnaryOperator::Not,
                    right_expression,
                )))
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> Result<Box<Expression>, LoxError> {
        match &self.tokens[self.index].token_type {
            TokenType::False => {
                self.index += 1;
                Ok(Box::new(Expression::Literal(LiteralType::False)))
            }
            TokenType::True => {
                self.index += 1;
                Ok(Box::new(Expression::Literal(LiteralType::True)))
            }
            TokenType::Nil => {
                self.index += 1;
                Ok(Box::new(Expression::Literal(LiteralType::Nil)))
            }
            TokenType::StringLiteral(value) => {
                self.index += 1;
                Ok(Box::new(Expression::Literal(LiteralType::String(
                    value.clone(),
                ))))
            }
            TokenType::NumberLiteral(value) => {
                self.index += 1;
                Ok(Box::new(Expression::Literal(LiteralType::Number(
                    value.clone(),
                ))))
            }
            TokenType::LeftParen => {
                // Start of a grouping
                self.index += 1;
                let expr = self.expression()?;
                match self.tokens[self.index].token_type {
                    TokenType::RightParen => {
                        self.index += 1;
                        Ok(Box::new(Expression::Grouping(expr)))
                    }
                    _ => Err(LoxError::ParserError(
                        ("Expected ')' after expression".to_string()),
                    )),
                }
            }
            _ => Err(LoxError::ParserError(format!(
                "Parser error, current index:{}",
                self.index
            ))),
        }
    }

    pub fn parse(&mut self) -> Result<Box<Expression>, LoxError> {
        // self.expression()
        !todo!("TODO");
    }
}
