use crate::parser::{Expression, Program, Statement};

#[allow(dead_code)]
pub fn visualize_program_ast(program: &Program) {
    for statement in &program.statements {
        handle_statement(statement);
    }
}

fn handle_statement(statement: &Statement) {
    match statement {
        Statement::Expression(expression) => {
            println!("Expression statement {}", stringify(&expression));
        }
        Statement::Print(expression) => {
            println!("Print statement: {}", stringify(&expression));
        }
        Statement::VariableDeclaration(variable_name, expression) => {
            let variable_name = std::str::from_utf8(variable_name).unwrap();
            println!(
                "Variable declaration var {} = {}",
                variable_name,
                stringify(&expression)
            );
        }
        Statement::Block(block_statements) => {
            println!("Block");
            for statement in block_statements {
                handle_statement(statement);
            }
        }
        Statement::If(condition, then_clause, else_clause) => {
            println!("If statement");
            println!("  condition:{}", stringify(condition.as_ref()));
            println!("  then statement:");
            handle_statement(then_clause.as_ref());
            match else_clause {
                Some(stmt) => {
                    println!("  else statement:");
                    handle_statement(stmt.as_ref())
                }
                None => {}
            }
        }
        Statement::While(condition, statement) => {
            println!("While statement");
            println!("  condition:{}", stringify(condition.as_ref()));
            println!("  statement:");
            handle_statement(statement.as_ref())
        }
    }
}

fn stringify(expression: &Expression) -> String {
    match &expression {
        Expression::Literal(literal_type) => {
            format!("({:?})", literal_type)
        }
        Expression::Unary(unary_operator, expression) => {
            format!("({:?} {:?})", unary_operator, stringify(&expression))
        }
        Expression::Binary(lhs, operator, rhs) => {
            format!(
                "({:?} {:?} , {:?})",
                operator,
                stringify(&lhs),
                stringify(&rhs)
            )
        }
        Expression::Grouping(expression) => {
            format!("(Grouping {:?})", stringify(&expression))
        }
        Expression::Identifier(name) => {
            let name_str = std::str::from_utf8(name).unwrap();
            format!("(Identifier {})", name_str)
        }
        Expression::Assignment(name, expression) => {
            let name_str = std::str::from_utf8(name).unwrap();
            format!("(Assignment {} = {:?})", name_str, stringify(&expression))
        }
        Expression::Logical(lhs, op, rhs) => {
            format!("(Logical {} {:?} {})", stringify(&lhs), op, stringify(&rhs))
        }
    }
}
