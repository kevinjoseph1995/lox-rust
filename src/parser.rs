use crate::{
    error::LoxError,
    tokens::{Token, TokenType},
};
use std::fmt::Debug;
use std::str;

const MAX_NUM_ARGUMENTS: usize = 64;

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

#[derive(Clone)]
pub enum Statement {
    Expression(Box<Expression>),
    Print(Box<Expression>),
    VariableDeclaration(Vec<u8>, Box<Expression>), // Identifier name and corresponding expression
    FunctionDeclaration(Vec<u8>, Vec<Vec<u8>>, Box<Statement>), // Function name, parameters and body
    Block(Vec<Statement>),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>), // (Condition, Then-clause, Else-clause)
    While(Box<Expression>, Box<Statement>),                      // Expr condition, Stmt body
}

#[derive(Clone)]
pub enum Expression {
    Literal(LiteralType),
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Logical(Box<Expression>, LogicalOperator, Box<Expression>),
    Grouping(Box<Expression>),
    Identifier(Vec<u8>),
    Assignment(Vec<u8>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>), // Callee and list of arguments
}

#[derive(Clone)]
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

#[derive(Clone)]
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
pub enum LogicalOperator {
    Or,
    And,
}

impl Debug for LogicalOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Or => write!(f, "or"),
            Self::And => write!(f, "and"),
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
    #[allow(dead_code)]
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
        declaration    → funDecl | varDecl |  statement ;
        statement      → exprStmt | printStmt | blockStmt | ifStmt;
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

    // declaration  → funDecl | varDecl   | statement ;
    fn declaration(&mut self) -> Result<Statement, LoxError> {
        match self.tokens[self.index].token_type {
            TokenType::Var => {
                // varDecl → "var" variable";" ;
                self.index += 1; // Move past the "var"
                self.variable()
            }
            TokenType::Fun => {
                // funDecl  → "fun" function ;
                self.index += 1; // Move past the "fun"
                self.function()
            }
            _ => self.statement(),
        }
    }

    // function → IDENTIFIER "(" parameters? ")" block ;
    fn function(&mut self) -> Result<Statement, LoxError> {
        let function_name;
        if let TokenType::Identifier(name) = &mut self.tokens[self.index].token_type {
            function_name = std::mem::take(name);
        } else {
            return Err(LoxError::ParserError(
                "Expected function identifier name after keyword \"fun\"".to_string(),
            ));
        }
        self.index += 1; // Move past the identifier

        if self.tokens[self.index].token_type != TokenType::LeftParen {
            return Err(LoxError::ParserError(
                "Function declaration expects \"(\" after identifier in function declaration"
                    .to_string(),
            ));
        }

        self.index += 1; // Move past the left parenthesis

        let mut parameters = Vec::new();
        loop {
            if let TokenType::Identifier(name) = &mut self.tokens[self.index].token_type {
                parameters.push(std::mem::take(name));
                self.index += 1; // Move past the identifier
            } else if self.tokens[self.index].token_type == TokenType::RightParen {
                self.index += 1; // Move past the right parenthesis
                break;
            } else {
                return Err(LoxError::ParserError(
                    "Error parsing function declaration".to_string(),
                ));
            }
            if self.tokens[self.index].token_type == TokenType::Comma {
                self.index += 1; // Consume the comma
            }
            if parameters.len() > MAX_NUM_ARGUMENTS {
                return Err(LoxError::ParserError(format!(
                    "Exceeded maximum number of parameters{}",
                    MAX_NUM_ARGUMENTS
                )));
            }
        }

        let function_body = self.statement()?;

        return Ok(Statement::FunctionDeclaration(
            function_name,
            parameters,
            Box::new(function_body),
        ));
    }

    // variable → IDENTIFIER ( "=" expression )? ";" ;
    fn variable(&mut self) -> Result<Statement, LoxError> {
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

    // statement      → exprStmt | printStmt | blockStmt | ifStmt | whileStmt;
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
            TokenType::If => {
                /*
                   ifStmt         → "if" "(" expression ")" statement
                                   ( "else" statement )? ;

                */
                self.index += 1;
                if self.tokens[self.index].token_type != TokenType::LeftParen {
                    return Err(LoxError::ParserError(
                        "If statement condition must be enclosed in parenthesis. Missing left paren".to_string(),
                    ));
                }
                self.index += 1;
                let condition = self.expression()?;
                if self.tokens[self.index].token_type != TokenType::RightParen {
                    return Err(LoxError::ParserError(
                        "If statement condition must be enclosed in parenthesis. Missing right paren".to_string(),
                    ));
                }
                self.index += 1;

                let then_statment = self.statement()?;

                if self.tokens[self.index].token_type == TokenType::Else {
                    self.index += 1;
                    let stmt = self.statement()?;
                    return Ok(Statement::If(
                        condition,
                        Box::new(then_statment),
                        Some(Box::new(stmt)),
                    ));
                }
                return Ok(Statement::If(condition, Box::new(then_statment), None));
            }
            TokenType::For => {
                /* We "desugar" the for loop into a block statement composed of a while statement and other expression statements
                    for(var i = 0; i < 10; i = i + 1)
                        print i;

                    is the same as:

                    {
                        var i = 0;
                        while (i < 10) {
                            print i;
                            i = i + 1;
                        }
                    }
                */
                self.index += 1;
                if self.tokens[self.index].token_type != TokenType::LeftParen {
                    return Err(LoxError::ParserError(
                        "For statement's initializer, condition and update expression should be enclosed in parenthesis. Mission left parenthesis".to_string(),
                    ));
                }
                // Initializer
                let mut initializer: Option<Statement> = None;
                self.index += 1;
                if self.tokens[self.index].token_type == TokenType::Semicolon {
                    self.index += 1; // Move past the semi-colon
                } else {
                    if self.tokens[self.index].token_type == TokenType::Var {
                        self.index += 1;
                        let initializer_stmt = self.variable()?;
                        initializer = Some(initializer_stmt);
                    } else {
                        let initializer_expr = self.get_expression_for_statement()?;
                        initializer = Some(Statement::Expression(initializer_expr));
                    }
                }
                // Condition
                let mut condition: Option<Box<Expression>> = None;
                if self.tokens[self.index].token_type == TokenType::Semicolon {
                    self.index += 1; // Move past the semi-colon
                } else {
                    let condition_expr = self.get_expression_for_statement()?;
                    condition = Some(condition_expr);
                }
                // Update
                let mut update: Option<Box<Expression>> = None;
                if self.tokens[self.index].token_type == TokenType::RightParen {
                    self.index += 1;
                } else {
                    let expr = self.expression()?;
                    if self.tokens[self.index].token_type != TokenType::RightParen {
                        return Err(LoxError::ParserError(
                            "For statement's initializer, condition and update expression should be enclosed in parenthesis. Mission right parenthesis".to_string(),
                        ));
                    }
                    update = Some(expr);
                    self.index += 1; // Move past the right paren
                }
                // Main body
                let body = self.statement()?;

                // Compose everything now
                let mut outer_block_statements: Vec<Statement> = Vec::new();
                if let Some(initializer_stmt) = initializer {
                    outer_block_statements.push(initializer_stmt);
                }
                if condition.is_none() {
                    condition = Some(Box::new(Expression::Literal(LiteralType::True)));
                }

                let mut inner_block_statements: Vec<Statement> = vec![body];
                if let Some(update_expr) = update {
                    inner_block_statements.push(Statement::Expression(update_expr));
                }

                outer_block_statements.push(Statement::While(
                    condition.unwrap(),
                    Box::new(Statement::Block(inner_block_statements)),
                ));

                return Ok(Statement::Block(outer_block_statements));
            }
            TokenType::While => {
                // whileStmt    → "while" "(" expression ")" statement ;
                self.index += 1;
                if self.tokens[self.index].token_type != TokenType::LeftParen {
                    return Err(LoxError::ParserError(
                        "While statement condition must be enclosed in parenthesis. Missing left parenthesis".to_string(),
                    ));
                }
                self.index += 1;
                let condition = self.expression()?;
                if self.tokens[self.index].token_type != TokenType::RightParen {
                    return Err(LoxError::ParserError(
                        "While statement condition must be enclosed in parenthesis. Missing right parenthesis".to_string(),
                    ));
                }
                self.index += 1;
                let statement = self.statement()?;

                return Ok(Statement::While(condition, Box::new(statement)));
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
                        | logic_or ;
        logic_or       → logic_and ( "or" logic_and )* ;
        logic_and      → equality ( "and" equality )* ;
        equality       → comparison ( ( "!=" | "==" ) comparison )* ;
        comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
        term           → factor ( ( "-" | "+" ) factor )* ;
        factor         → unary ( ( "/" | "*" ) unary )* ;
        unary          → ( "!" | "-" ) unary | call ;
        call           → primary ( "(" arguments? ")" )* ;
        arguments      → expression ( "," expression )* ;
        primary        → NUMBER | STRING | "true" | "false" | "nil"
                    | "(" expression ")" ;
    */

    fn expression(&mut self) -> Result<Box<Expression>, LoxError> {
        return self.assignment();
    }

    // assignment     → IDENTIFIER "=" assignment | equality ;
    fn assignment(&mut self) -> Result<Box<Expression>, LoxError> {
        let mut lhs = self.logic_or()?;

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
                "Expected L-value for assignment".to_string(),
            ));
        }

        return Ok(lhs);
    }

    // logic_or → logic_and ( "or" logic_and )* ;
    fn logic_or(&mut self) -> Result<Box<Expression>, LoxError> {
        let lhs = self.logic_and()?;
        if let TokenType::Or = self.tokens[self.index].token_type {
            self.index += 1;
            let rhs = self.logic_and()?;
            return Ok(Box::new(Expression::Logical(lhs, LogicalOperator::Or, rhs)));
        }
        return Ok(lhs);
    }

    // logic_and → equality ( "and" equality )* ;
    fn logic_and(&mut self) -> Result<Box<Expression>, LoxError> {
        let lhs = self.equality()?;
        if let TokenType::And = self.tokens[self.index].token_type {
            self.index += 1;
            let rhs = self.equality()?;
            return Ok(Box::new(Expression::Logical(
                lhs,
                LogicalOperator::And,
                rhs,
            )));
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

    // unary → ( "!" | "-" ) unary | call ;
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
            _ => self.call(),
        }
    }

    // call → primary ( "(" arguments? ")" )* ;
    fn call(&mut self) -> Result<Box<Expression>, LoxError> {
        let expr = self.primary()?;
        if self.tokens[self.index].token_type == TokenType::LeftParen {
            self.index += 1; // Move past the left parenthesis
            let arguments = self.arguments()?;
            return Ok(Box::new(Expression::Call(expr, arguments)));
        }
        return Ok(expr);
    }

    fn arguments(&mut self) -> Result<Vec<Expression>, LoxError> {
        if self.tokens[self.index].token_type == TokenType::RightParen {
            self.index += 1; // Move past the right parenthesis
            return Ok(Vec::new());
        }
        // We have a hard-limit for the number of arguments for a "call"
        let mut args: Vec<Expression> = Vec::new();
        for _ in 0..MAX_NUM_ARGUMENTS + 1 {
            let arg = self.expression()?;
            args.push(arg.as_ref().clone());
            if self.tokens[self.index].token_type == TokenType::Comma {
                self.index += 1;
                if self.tokens[self.index].token_type == TokenType::RightParen {
                    // Allow trailing commas
                    self.index += 1; // Move past the right paren
                    return Ok(args);
                }
            } else if self.tokens[self.index].token_type == TokenType::RightParen {
                self.index += 1; // Move past the right paren
                return Ok(args);
            } else {
                return Err(LoxError::ParserError(format!(
                    "Parsing arguments of function call line: {}",
                    self.tokens[self.index].line_number
                )));
            }
        }
        return Err(LoxError::ParserError(format!(
            "Reached maximum number of arguments that can be passed to a call expression"
        )));
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
