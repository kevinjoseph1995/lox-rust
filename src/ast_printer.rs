use crate::parser::{Expression, Program, Statement};

#[allow(dead_code)]
pub fn visualize_program_ast(program: &Program) {
    for statement in &program.statements {
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
                    "Varaible declaration var {} = {}",
                    variable_name,
                    stringify(&expression)
                );
            }
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
    }
}
