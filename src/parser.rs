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
    VariableDeclaration(Vec<u8>, Box<Expression>), // Identifier name and corresponding expression
    Block(Vec<Statement>),
}

pub enum Expression {
    Literal(LiteralType),
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Grouping(Box<Expression>),
    Identifier(Vec<u8>),
    Assignment(Vec<u8>, Box<Expression>),
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

#[derive(Clone)]
pub enum LiteralType {
    Number(f64),
    String(Vec<u8>),
    True,
    False,
    Nil,
}

impl LiteralType {
    pub fn take(&mut self) -> Self {
        match self {
            Self::Number(arg0) => Self::Number(std::mem::take(arg0)),
            Self::String(arg0) => Self::String(std::mem::take(arg0)),
            Self::True => Self::True,
            Self::False => Self::False,
            Self::Nil => Self::Nil,
        }
    }
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

impl PartialEq for LiteralType {
    fn eq(&self, other: &Self) -> bool {
        if let (LiteralType::Number(value1), LiteralType::Number(value2)) = (&self, &other) {
            return value1 == value2;
        }
        if let (LiteralType::String(value1), LiteralType::String(value2)) = (&self, &other) {
            return value1 == value2;
        }
        return self == other;
    }
}

impl From<bool> for LiteralType {
    fn from(v: bool) -> LiteralType {
        if v {
            LiteralType::True
        } else {
            LiteralType::False
        }
    }
}

impl std::fmt::Display for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralType::Number(number) => {
                write!(f, "{}", number)
            }
            LiteralType::String(u8_vec) => {
                let result = std::str::from_utf8(u8_vec);

                match result {
                    Ok(str_val) => write!(f, "{}", str_val),
                    Err(_) => write!(f, "UTF decoding error"),
                }
            }
            LiteralType::True => {
                write!(f, "True")
            }
            LiteralType::False => {
                write!(f, "False")
            }
            LiteralType::Nil => {
                write!(f, "Nil")
            }
        }
    }
}

fn report_error(error: LoxError, current_token: Option<&Token>) {
    if let Some(token) = current_token {
        eprintln!(
            "\x1b[1;31mError: {}. Processing token on line {} Col-[{}:{}] \x1b[0m",
            error,
            token.line_number,
            token.start,
            token.start + token.length
        );
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, index: 0 }
    }

    pub fn parse(&mut self) -> Result<Program, LoxError> {
        self.program()
    }

    /*
        program        → declaration* EOF ;
        declaration    → varDecl |  statement ;
        statement      → exprStmt | printStmt ;
    */
    fn program(&mut self) -> Result<Program, LoxError> {
        let mut program = Program::new();
        let mut found_error = false;
        loop {
            if self.index == self.tokens.len()
                || self.tokens[self.index].token_type == TokenType::Eof
            {
                break;
            } else {
                match self.declaration() {
                    Ok(statement) => program.statements.push(statement),
                    Err(error) => {
                        if self.index < self.tokens.len() {
                            report_error(error, Some(&self.tokens[self.index]))
                        } else {
                            report_error(error, None);
                        }
                        found_error = true;
                        self.index += 1;
                    }
                }
            }
        }
        if found_error {
            return Err(LoxError::ParserError("Found parse error".to_string()));
        }
        Ok(program)
    }

    // declaration  → varDecl   | statement ;
    fn declaration(&mut self) -> Result<Statement, LoxError> {
        match self.tokens[self.index].token_type {
            TokenType::Var => {
                self.index += 1;
                self.variable_declaration()
            }
            _ => self.statement(),
        }
    }

    // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn variable_declaration(&mut self) -> Result<Statement, LoxError> {
        // We expect an identifier after the "var keyword"
        let identifier_name;
        match &mut self.tokens[self.index].token_type {
            TokenType::Identifier(name) => {
                self.index += 1; // Move past the identifier name
                identifier_name = std::mem::take(name);
            }
            _ => {
                return Err(LoxError::ParserError(
                    "Expected identifier after keyword \"var\" in variable declaration".to_string(),
                ))
            }
        }

        match self.tokens[self.index].token_type {
            TokenType::Equal => {
                self.index += 1;
            }
            _ => {
                return Err(LoxError::ParserError(
                    "Expected \"=\" after identifier name in variable declaration".to_string(),
                ))
            }
        }
        let expression = self.expression()?;
        match self.tokens[self.index].token_type {
            TokenType::Semicolon => {
                self.index += 1;
            }
            _ => {
                return Err(LoxError::ParserError(
                    "Expected \";\" at the end of variable declaration".to_string(),
                ))
            }
        }
        Ok(Statement::VariableDeclaration(identifier_name, expression))
    }

    // statement → exprStmt | printStmt | block;
    fn statement(&mut self) -> Result<Statement, LoxError> {
        match self.tokens[self.index].token_type {
            TokenType::Print => {
                // printStmt -> "print" expression ";"
                self.index += 1;
                let expr = self.get_expression_for_statement()?;
                return Ok(Statement::Print(expr));
            }
            TokenType::LeftBrace => {
                self.index += 1;
                let block_statements = self.block()?;
                return Ok(Statement::Block(block_statements));
            }

            _ => {
                // exprStmt -> expression ";"
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
    // block  → "{" declaration* "}" ;
    fn block(&mut self) -> Result<Vec<Statement>, LoxError> {
        let mut declarations: Vec<Statement> = Vec::new();
        let left_paren_index = self.index - 1;
        assert!(self.tokens[left_paren_index].token_type == TokenType::LeftBrace);
        loop {
            match self.tokens[self.index].token_type {
                TokenType::RightBrace => {
                    self.index += 1;
                    return Ok(declarations);
                }
                TokenType::Eof => {
                    return Err(LoxError::ParserError(
                       format!( "Unterminated block, missing a \"}}\", corresponding \"{{\" is found on line {}", self.tokens[left_paren_index].line_number),
                    ));
                }
                _ => {
                    let new_declaration = self.declaration()?;
                    declarations.push(new_declaration);
                }
            }
        }
    }

    /*
        Expression grammar for Lox from https://craftinginterpreters.com/parsing-expressions.html
        expression     → assignment ;
        assignment     → IDENTIFIER "=" assignment
                        | equality ;
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
        return self.assignment();
    }

    // assignment     → IDENTIFIER "=" assignment | equality ;
    fn assignment(&mut self) -> Result<Box<Expression>, LoxError> {
        let mut lhs = self.equality()?;

        if self.tokens[self.index].token_type == TokenType::Equal {
            self.index += 1; // Move past the "="
            let value = self.assignment()?;
            // Validate that the LHS is an expression that we can assign to. For now only variable identifiers are assignable
            if let Expression::Identifier(name) = lhs.as_mut() {
                return Ok(Box::new(Expression::Assignment(
                    std::mem::take(name),
                    value,
                )));
            }
            // The LHS wasn't an L-value, returning error here, maybe don't?
            return Err(LoxError::ParserError(
                "Expected L-value for assignemnt".to_string(),
            ));
        }

        return Ok(lhs);
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
     * primary → NUMBER | STRING | "true" | "false" | "nil"  | "(" expression ") | IDENTIFIER"
     */
    fn primary(&mut self) -> Result<Box<Expression>, LoxError> {
        match &mut self.tokens[self.index].token_type {
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
                    std::mem::take(value),
                ))))
            }
            TokenType::NumberLiteral(value) => {
                self.index += 1;
                Ok(Box::new(Expression::Literal(LiteralType::Number(
                    std::mem::take(value),
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
            TokenType::Identifier(identifier_name) => {
                self.index += 1;
                Ok(Box::new(Expression::Identifier(std::mem::take(
                    identifier_name,
                ))))
            }
            _ => Err(LoxError::ParserError(format!(
                "Parser error, current index:{}, line number:{}",
                self.index, &self.tokens[self.index].line_number
            ))),
        }
    }
}
