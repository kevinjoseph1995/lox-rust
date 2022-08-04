use crate::{
    error::LoxError,
    parser::{Expression, Program, Statement},
};
use std::collections::HashMap;

pub type DistanceTable = Vec<Option<usize>>;

type ScopeStack = Vec<HashMap<String, bool>>;

#[derive(Copy, Clone, PartialEq)]
pub enum FunctionType {
    Function,
    Method,
    None,
}

#[derive(Copy, Clone, PartialEq)]
enum ClassType {
    Class,
    None,
}

pub struct Resolver {
    current_function_type: FunctionType,
    current_class_type: ClassType,
    local_variable_lookup_distance_table: DistanceTable,
    scopes: ScopeStack,
}

impl Resolver {
    fn declare(&mut self, name: &String) {
        if self.scopes.is_empty() {
            return;
        }
        self.scopes.last_mut().unwrap().insert(name.clone(), false);
    }
    fn define(&mut self, name: &String) {
        if self.scopes.is_empty() {
            return;
        }
        if let Some(defined) = self.scopes.last_mut().unwrap().get_mut(name) {
            *defined = true;
        } else {
            assert!(false);
        }
    }
    pub fn new() -> Self {
        Self {
            current_function_type: FunctionType::None,
            current_class_type: ClassType::None,
            local_variable_lookup_distance_table: Vec::new(),
            scopes: Vec::new(),
        }
    }
    pub fn resolve_variables(&mut self, program: &Program) -> Result<DistanceTable, LoxError> {
        self.local_variable_lookup_distance_table = vec![None; program.num_of_identifiers as usize];
        self.current_function_type = FunctionType::None;
        self.current_class_type = ClassType::None;
        self.scopes.clear();
        for statement in &program.statements {
            self.visit_statement(statement)?;
        }
        Ok(self.local_variable_lookup_distance_table.clone())
    }

    fn visit_statement(&mut self, statement: &Statement) -> Result<(), LoxError> {
        match statement {
            Statement::Block(statements) => {
                self.scopes.push(HashMap::new());
                for statement in statements {
                    self.visit_statement(statement)?;
                }
                self.scopes.pop();
                Ok(())
            }
            Statement::VariableDeclaration(name, expression) => {
                self.declare(name);
                self.visit_expression(expression)?;
                self.define(name);
                Ok(())
            }
            Statement::FunctionDeclaration(name, parameters, body) => {
                let enclosing_function_type = self.current_function_type;
                self.current_function_type = FunctionType::Function;
                self.declare(name);
                self.define(name);
                self.scopes.push(HashMap::new());
                for param in parameters {
                    self.declare(param);
                    self.define(param);
                }
                self.visit_statement(body.as_ref())?;
                self.scopes.pop();
                self.current_function_type = enclosing_function_type;
                Ok(())
            }
            Statement::Expression(expr) => {
                self.visit_expression(expr)?;
                Ok(())
            }
            Statement::Println(expr) => {
                self.visit_expression(expr)?;
                Ok(())
            }
            Statement::If(condition, then, else_optional) => {
                self.visit_expression(condition)?;
                self.visit_statement(then.as_ref())?;
                if let Some(else_stmt) = else_optional {
                    self.visit_statement(else_stmt.as_ref())?;
                }
                Ok(())
            }
            Statement::While(condition, body) => {
                self.visit_expression(condition)?;
                self.visit_statement(body.as_ref())?;
                Ok(())
            }
            Statement::Print(expr) => {
                self.visit_expression(expr)?;
                Ok(())
            }
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expression(expr)?;
                }
                Ok(())
            }
            Statement::ClassDeclaration(name, member_functions) => {
                let enclosing_class_type = self.current_class_type;
                self.current_class_type = ClassType::Class;
                self.declare(name);
                self.define(name);
                for stmt in member_functions {
                    match stmt {
                        Statement::FunctionDeclaration(function_name, parameters, body) => {
                            let enclosing_function_type = self.current_function_type;
                            self.current_function_type = FunctionType::Method;
                            self.scopes.push(HashMap::new());
                            self.scopes
                                .push(HashMap::from([(function_name.clone(), true)]));
                            self.scopes
                                .push(HashMap::from([("this".to_string(), true)]));
                            for param in parameters {
                                self.declare(param);
                                self.define(param);
                            }
                            self.visit_statement(body.as_ref())?;
                            self.scopes.pop();
                            self.scopes.pop();
                            self.current_function_type = enclosing_function_type;
                        }
                        _ => {
                            return Err(LoxError::ParserError(
                                "Only functional declarations are allowed inside class block"
                                    .to_string(),
                            ))
                        }
                    }
                }
                self.scopes.pop();
                self.current_class_type = enclosing_class_type;
                Ok(())
            }
        }
    }
    fn visit_expression(&mut self, expression: &Expression) -> Result<(), LoxError> {
        match expression {
            Expression::Literal(_) => Ok(()),
            Expression::Unary(_, expr) => self.visit_expression(expr),
            Expression::Binary(expr1, _, expr2) => {
                self.visit_expression(expr1)?;
                self.visit_expression(expr2)?;
                Ok(())
            }
            Expression::Logical(expr1, _, expr2) => {
                self.visit_expression(expr1)?;
                self.visit_expression(expr2)?;
                Ok(())
            }
            Expression::Grouping(expr) => self.visit_expression(expr),
            Expression::Identifier(id, name) => {
                if !self.scopes.is_empty() {
                    if let Some(defined) = self.scopes.last().unwrap().get(name) {
                        if !defined {
                            return Err(LoxError::ParserError(
                                "Can't read local variable in its own initializer.".to_string(),
                            ));
                        }
                    }
                }
                for (distance, scope) in self.scopes.iter().rev().enumerate() {
                    if scope.contains_key(name) {
                        self.local_variable_lookup_distance_table[*id as usize] = Some(distance);
                        break;
                    }
                }
                Ok(())
            }
            Expression::Assignment(id, name, expression) => {
                self.visit_expression(expression)?;
                for (distance, scope) in self.scopes.iter().rev().enumerate() {
                    if scope.contains_key(name) {
                        self.local_variable_lookup_distance_table[*id as usize] = Some(distance);
                    }
                }
                Ok(())
            }
            Expression::Call(callee_expr, arg_expression) => {
                self.visit_expression(callee_expr)?;
                for arg in arg_expression {
                    self.visit_expression(arg)?;
                }
                Ok(())
            }
            Expression::Get(expression, _name) => {
                self.visit_expression(expression)?;
                Ok(())
            }
            Expression::Set(lhs, _, rhs) => {
                self.visit_expression(lhs)?;
                self.visit_expression(rhs)?;
                Ok(())
            }
            Expression::This(id) => {
                if self.scopes.is_empty() {
                    return Err(LoxError::ParserError(
                        "Found this in global scope".to_string(),
                    ));
                }
                if self.current_class_type != ClassType::Class {
                    return Err(LoxError::ParserError(
                        "Found this outside of class declaration".to_string(),
                    ));
                }
                for (distance, scope) in self.scopes.iter().rev().enumerate() {
                    if scope.contains_key("this") {
                        self.local_variable_lookup_distance_table[*id as usize] = Some(distance);
                        break;
                    }
                }
                Ok(())
            }
        }
    }
}
