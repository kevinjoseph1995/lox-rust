use crate::parser::{Expression, Program, Statement};

#[allow(dead_code)]
pub fn visualize_program_ast(program: &Program) {
    for statement in &program.statements {
        match statement {
            Statement::Expression(expression) => {
                stringify(&expression);
            }
            Statement::Print(expression) => {
                stringify(&expression);
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
    }
}
