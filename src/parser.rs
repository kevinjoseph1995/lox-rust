use crate::{
    error::LoxError,
    tokens::{Token, TokenType},
};
use std::fmt::Debug;
use std::str;

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::<Statement>::new(),
        }
    }
}

pub enum Statement {
    Expression(Box<Expression>),
    Print(Box<Expression>),
}

#[derive(Debug)]
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

impl Debug for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Negate => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
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

impl Debug for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Equal => write!(f, "="),
            Self::NotEqual => write!(f, "!="),
            Self::LessThan => write!(f, "<"),
            Self::LessThanEqual => write!(f, "<="),
            Self::GreaterThan => write!(f, ">"),
            Self::GreaterThanEqual => write!(f, ">="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
        }
    }
}

pub enum LiteralType {
    Number(f64),
    String(Vec<u8>),
    True,
    False,
    Nil,
}

impl Debug for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(arg0) => write!(f, "{}", arg0),
            Self::String(arg0) => write!(f, "{}", str::from_utf8(arg0).unwrap()),
            Self::True => write!(f, "True"),
            Self::False => write!(f, "False"),
            Self::Nil => write!(f, "Nil"),
        }
    }
}

pub struct Parser<'a> {
    tokens: &'a [Token], // The tokens are owned by the scanner
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn new<'b: 'a>(tokens: &'b [Token]) -> Self {
        Parser { tokens, index: 0 }
    }

    pub fn parse(&mut self) -> Result<Program, LoxError> {
        self.program()
    }
    /*
        Taken from https://craftinginterpreters.com/statements-and-state.html
        program        → statement* EOF ;
        statement      → exprStmt  | printStmt ;
        exprStmt       → expression ";" ;
        printStmt      → "print" expression ";" ;

    */

    fn program(&mut self) -> Result<Program, LoxError> {
        let mut program = Program::new();
        loop {
            if self.index == self.tokens.len()
                || self.tokens[self.index].token_type == TokenType::Eof
            {
                break;
            } else {
                let statement = self.statement()?;
                program.statements.push(statement)
            }
        }
        Ok(program)
    }

    fn statement(&mut self) -> Result<Statement, LoxError> {
        match self.tokens[self.index].token_type {
            TokenType::Print => {
                self.index += 1;
                let expr = self.get_expression_for_statement()?;
                return Ok(Statement::Print(expr));
            }
            _ => {
                let expr = self.get_expression_for_statement()?;
                return Ok(Statement::Expression(expr));
            }
        };
    }

    fn get_expression_for_statement(&mut self) -> Result<Box<Expression>, LoxError> {
        let expression = self.expression()?;
        match self.tokens[self.index].token_type {
            TokenType::Semicolon => {
                self.index += 1; // Move the index past the semicolon token
                Ok(expression)
            }
            _ => {
                return Err(LoxError::ParserError(format!(
                    "Statement must terminate with a semicolon. Line number:{}",
                    &self.tokens[self.index].line_number
                )))
            }
        }
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

    fn expression(&mut self) -> Result<Box<Expression>, LoxError> {
        return self.equality();
    }

    /*
       equality → comparison ( ( "!=" | "==" ) comparison )*
    */
    fn equality(&mut self) -> Result<Box<Expression>, LoxError> {
        let expr = self.comparison()?;
        loop {
            match self.tokens[self.index].token_type {
                TokenType::BangEqual => {
                    self.index += 1;
                    let right_expr = self.comparison()?;
                    return Ok(Box::new(Expression::Binary(
                        expr,
                        BinaryOperator::NotEqual,
                        right_expr,
                    )));
                }
                TokenType::EqualEqual => {
                    self.index += 1;
                    let right_expr = self.comparison()?;
                    return Ok(Box::new(Expression::Binary(
                        expr,
                        BinaryOperator::Equal,
                        right_expr,
                    )));
                }
                _ => {
                    break;
                }
            }
        }
        Ok(expr)
    }

    /*
     *comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
     */
    fn comparison(&mut self) -> Result<Box<Expression>, LoxError> {
        let expr = self.term()?;
        loop {
            match self.tokens[self.index].token_type {
                TokenType::Greater => {
                    self.index += 1;
                    let right_expr = self.term()?;
                    return Ok(Box::new(Expression::Binary(
                        expr,
                        BinaryOperator::GreaterThan,
                        right_expr,
                    )));
                }
                TokenType::GreaterEqual => {
                    self.index += 1;
                    let right_expr = self.term()?;
                    return Ok(Box::new(Expression::Binary(
                        expr,
                        BinaryOperator::GreaterThanEqual,
                        right_expr,
                    )));
                }
                TokenType::Less => {
                    self.index += 1;
                    let right_expr = self.term()?;
                    return Ok(Box::new(Expression::Binary(
                        expr,
                        BinaryOperator::LessThan,
                        right_expr,
                    )));
                }
                TokenType::LessEqual => {
                    self.index += 1;
                    let right_expr = self.term()?;
                    return Ok(Box::new(Expression::Binary(
                        expr,
                        BinaryOperator::LessThanEqual,
                        right_expr,
                    )));
                }
                _ => {
                    break;
                }
            }
        }
        Ok(expr)
    }

    /*
    term → factor ( ( "-" | "+" ) factor )*
    */
    fn term(&mut self) -> Result<Box<Expression>, LoxError> {
        let expr = self.factor()?;
        loop {
            match self.tokens[self.index].token_type {
                TokenType::Minus => {
                    self.index += 1;
                    let right_expr = self.factor()?;
                    return Ok(Box::new(Expression::Binary(
                        expr,
                        BinaryOperator::Minus,
                        right_expr,
                    )));
                }
                TokenType::Plus => {
                    self.index += 1;
                    let right_expr = self.factor()?;
                    return Ok(Box::new(Expression::Binary(
                        expr,
                        BinaryOperator::Plus,
                        right_expr,
                    )));
                }
                _ => {
                    break;
                }
            }
        }
        return Ok(expr);
    }

    /*
     * factor → unary ( ( "/" | "*" ) unary )*
     */
    fn factor(&mut self) -> Result<Box<Expression>, LoxError> {
        let expr = self.unary()?;
        loop {
            match self.tokens[self.index].token_type {
                TokenType::Slash => {
                    self.index += 1;
                    let right_expr = self.unary()?;
                    return Ok(Box::new(Expression::Binary(
                        expr,
                        BinaryOperator::Divide,
                        right_expr,
                    )));
                }
                TokenType::Star => {
                    self.index += 1;
                    let right_expr = self.unary()?;
                    return Ok(Box::new(Expression::Binary(
                        expr,
                        BinaryOperator::Multiply,
                        right_expr,
                    )));
                }
                _ => {
                    break;
                }
            }
        }
        return Ok(expr);
    }

    /*
     * unary → ( "!" | "-" ) unary     | primary ;
     */
    fn unary(&mut self) -> Result<Box<Expression>, LoxError> {
        match self.tokens[self.index].token_type {
            TokenType::Bang => {
                self.index += 1;
                let right_expr = self.unary()?;
                Ok(Box::new(Expression::Unary(UnaryOperator::Not, right_expr)))
            }
            TokenType::Minus => {
                self.index += 1;
                let right_expr = self.unary()?;
                Ok(Box::new(Expression::Unary(
                    UnaryOperator::Negate,
                    right_expr,
                )))
            }
            _ => self.primary(),
        }
    }

    /*
     * primary → NUMBER | STRING | "true" | "false" | "nil"  | "(" expression ")"
     */
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
                    value.clone(), // Would have been nice to reuse the allocation made for the string by the scanner
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
                        "Expected ')' after expression".to_string(),
                    )),
                }
            }
            _ => Err(LoxError::ParserError(format!(
                "Parser error, current index:{}, line number:{}",
                self.index, &self.tokens[self.index].line_number
            ))),
        }
    }
}
