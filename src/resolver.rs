use crate::{
    error::LoxError,
    parser::{Expression, Program, Statement},
};
use std::collections::HashMap;

pub type DistanceTable = Vec<Option<usize>>;

type ScopeStack = Vec<HashMap<String, bool>>;

pub fn resolve_variables(program: &Program) -> Result<DistanceTable, LoxError> {
    let mut local_variable_lookup_distance_table: DistanceTable =
        vec![None; program.num_of_identifiers as usize];
    let mut scopes: ScopeStack = Vec::new();
    for statement in &program.statements {
        visit_statement(
            statement,
            &mut scopes,
            &mut local_variable_lookup_distance_table,
        )?;
    }
    Ok(local_variable_lookup_distance_table)
}

fn declare(scopes: &mut ScopeStack, name: &String) {
    if scopes.is_empty() {
        return;
    }
    scopes.last_mut().unwrap().insert(name.clone(), false);
}

fn define(scopes: &mut ScopeStack, name: &String) {
    if scopes.is_empty() {
        return;
    }
    if let Some(defined) = scopes.last_mut().unwrap().get_mut(name) {
        *defined = true;
    } else {
        assert!(false);
    }
}

fn visit_statement(
    statement: &Statement,
    scopes: &mut ScopeStack,
    local_table: &mut DistanceTable,
) -> Result<(), LoxError> {
    match statement {
        Statement::Block(statements) => {
            scopes.push(HashMap::new());
            for statement in statements {
                visit_statement(statement, scopes, local_table)?;
            }
            scopes.pop();
            Ok(())
        }
        Statement::VariableDeclaration(name, expression) => {
            declare(scopes, name);
            visit_expression(expression, scopes, local_table)?;
            define(scopes, name);
            Ok(())
        }
        Statement::FunctionDeclaration(name, parameters, body) => {
            declare(scopes, name);
            define(scopes, name);
            scopes.push(HashMap::new());
            for param in parameters {
                declare(scopes, param);
                define(scopes, param);
            }
            visit_statement(body.as_ref(), scopes, local_table)?;
            scopes.pop();
            Ok(())
        }
        Statement::Expression(expr) => {
            visit_expression(expr, scopes, local_table)?;
            Ok(())
        }
        Statement::Println(expr) => {
            visit_expression(expr, scopes, local_table)?;
            Ok(())
        }
        Statement::If(condition, then, else_optional) => {
            visit_expression(condition, scopes, local_table)?;
            visit_statement(then.as_ref(), scopes, local_table)?;
            if let Some(else_stmt) = else_optional {
                visit_statement(else_stmt.as_ref(), scopes, local_table)?;
            }
            Ok(())
        }
        Statement::While(condition, body) => {
            visit_expression(condition, scopes, local_table)?;
            visit_statement(body.as_ref(), scopes, local_table)?;
            Ok(())
        }
        Statement::Print(expr) => {
            visit_expression(expr, scopes, local_table)?;
            Ok(())
        }
        Statement::Return(expr) => {
            if let Some(expr) = expr {
                visit_expression(expr, scopes, local_table)?;
            }
            Ok(())
        }
        Statement::ClassDeclaration(name, member_functions) => {
            declare(scopes, name);
            define(scopes, name);
            for stmt in member_functions {
                match stmt {
                    Statement::FunctionDeclaration(function_name, parameters, body) => {
                        scopes.push(HashMap::new());
                        scopes.push(HashMap::from([(function_name.clone(), true)]));
                        scopes.push(HashMap::from([("this".to_string(), true)]));
                        for param in parameters {
                            declare(scopes, param);
                            define(scopes, param);
                        }
                        visit_statement(body.as_ref(), scopes, local_table)?;
                        scopes.pop();
                        scopes.pop();
                    }
                    _ => {
                        return Err(LoxError::ParserError(
                            "Only functional declarations are allowed inside class block"
                                .to_string(),
                        ))
                    }
                }
            }
            scopes.pop();
            Ok(())
        }
    }
}

fn visit_expression(
    expression: &Expression,
    scopes: &ScopeStack,
    local_table: &mut DistanceTable,
) -> Result<(), LoxError> {
    match expression {
        Expression::Literal(_) => Ok(()),
        Expression::Unary(_, expr) => visit_expression(expr, scopes, local_table),
        Expression::Binary(expr1, _, expr2) => {
            visit_expression(expr1, scopes, local_table)?;
            visit_expression(expr2, scopes, local_table)?;
            Ok(())
        }
        Expression::Logical(expr1, _, expr2) => {
            visit_expression(expr1, scopes, local_table)?;
            visit_expression(expr2, scopes, local_table)?;
            Ok(())
        }
        Expression::Grouping(expr) => visit_expression(expr, scopes, local_table),
        Expression::Identifier(id, name) => {
            if !scopes.is_empty() {
                if let Some(defined) = scopes.last().unwrap().get(name) {
                    if !defined {
                        return Err(LoxError::ParserError(
                            "Can't read local variable in its own initializer.".to_string(),
                        ));
                    }
                }
            }
            for (distance, scope) in scopes.iter().rev().enumerate() {
                if scope.contains_key(name) {
                    local_table[*id as usize] = Some(distance);
                    break;
                }
            }
            Ok(())
        }
        Expression::Assignment(id, name, expression) => {
            visit_expression(expression, scopes, local_table)?;
            for (distance, scope) in scopes.iter().rev().enumerate() {
                if scope.contains_key(name) {
                    local_table[*id as usize] = Some(distance);
                }
            }
            Ok(())
        }
        Expression::Call(callee_expr, arg_expression) => {
            visit_expression(callee_expr, scopes, local_table)?;
            for arg in arg_expression {
                visit_expression(arg, scopes, local_table)?;
            }
            Ok(())
        }
        Expression::Get(expression, _name) => {
            visit_expression(expression, scopes, local_table)?;
            Ok(())
        }
        Expression::Set(lhs, _, rhs) => {
            visit_expression(lhs, scopes, local_table)?;
            visit_expression(rhs, scopes, local_table)?;
            Ok(())
        }
        Expression::This(id) => {
            if scopes.is_empty() {
                return Err(LoxError::ParserError(
                    "Found this in global scope".to_string(),
                ));
            }
            for (distance, scope) in scopes.iter().rev().enumerate() {
                if scope.contains_key("this") {
                    local_table[*id as usize] = Some(distance);
                    break;
                }
            }

            Ok(())
        }
    }
}
