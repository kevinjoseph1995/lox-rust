use crate::parser::Expression;

pub fn print_expression(expression: &Expression) {
    println!("{}", stringify(expression));
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
