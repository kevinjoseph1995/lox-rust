use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use crate::error::LoxError;
use crate::parser::{
    BinaryOperator, Expression, LogicalOperator, Program, Statement, UnaryOperator,
};
use crate::resolver;
use crate::resolver::DistanceTable;
use crate::runtime::add_child;
use crate::runtime::create_instance;
use crate::runtime::is_true_value;
use crate::runtime::CallableObject;
use crate::runtime::EnvironmentNode;
use crate::runtime::LoxClass;
use crate::runtime::Object;

pub struct Interpreter {
    global_environment: Rc<RefCell<EnvironmentNode>>, // Root of our environment tree. Keeps all the other nodes alive
    current: Weak<RefCell<EnvironmentNode>>, // Points to the current environment based on what state the interpreter's is in
    local_variable_resolution_table: DistanceTable,
}

impl Interpreter {
    pub fn new() -> Self {
        let global_environment = Rc::new(RefCell::new(EnvironmentNode::new()));
        let weak_ptr_to_global = Rc::downgrade(&global_environment);
        Interpreter {
            global_environment,
            current: weak_ptr_to_global,
            local_variable_resolution_table: Vec::new(),
        }
    }

    pub fn interpret(&mut self, program: Program) -> Result<(), LoxError> {
        self.local_variable_resolution_table = resolver::resolve_variables(&program)?;
        let mut statements = program.statements;
        for statement in &mut statements {
            self.handle_statement(statement)?;
        }
        Ok(())
    }

    pub fn handle_statement(&mut self, statement: &Statement) -> Result<(), LoxError> {
        match statement {
            Statement::Expression(expression) => {
                self.evaluate(expression)?;
            }
            Statement::Print(expression) => {
                let result = self.evaluate(expression)?;
                print!("{}", result);
            }
            Statement::VariableDeclaration(name, expression) => {
                let value = self.evaluate(expression)?;
                self.update_or_add(name, value);
            }
            Statement::Block(block_statements) => {
                // Entered new scope
                let prev = self.current.clone();
                self.current = add_child(&mut self.current.upgrade().unwrap());
                for statement in block_statements {
                    self.handle_statement(statement)?;
                }
                self.current = prev;
            }
            Statement::If(condition, then_clause, else_clause) => {
                let condition_value = self.evaluate(condition)?;
                if is_true_value(&condition_value) {
                    self.handle_statement(then_clause)?;
                } else {
                    if let Some(else_clause) = else_clause {
                        self.handle_statement(else_clause)?;
                    }
                }
            }
            Statement::While(condition_expr, statement) => {
                let mut condition_value = self.evaluate(condition_expr)?;
                while is_true_value(&condition_value) {
                    self.handle_statement(statement)?;
                    condition_value = self.evaluate(condition_expr)?;
                }
            }
            Statement::FunctionDeclaration(name, parameters, body) => {
                let callable = CallableObject::new(name, parameters, &body, &self.current.clone());
                self.update_or_add(name, Object::Callable(callable));
            }
            Statement::Return(exp_opt) => {
                if let Some(expression) = exp_opt {
                    let value = self.evaluate(expression)?;
                    return Err(LoxError::Return(value));
                } else {
                    return Err(LoxError::Return(Object::Nil));
                }
            }
            Statement::Println(expression) => {
                let result = self.evaluate(expression)?;
                println!("{}", result)
            }
            Statement::ClassDeclaration(name, _member_functions) => {
                let mut methods: HashMap<String, CallableObject> = HashMap::new();
                for stmt in _member_functions {
                    match stmt {
                        Statement::FunctionDeclaration(name, params, body) => {
                            let callable =
                                CallableObject::new(name, params, &body, &self.current.clone());
                            methods.insert(name.clone(), callable);
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Only function declarations can be found in class body".to_string(),
                            ))
                        }
                    }
                }
                let class_object = LoxClass {
                    name: name.clone(),
                    methods,
                };
                self.update_or_add(name, Object::Class(Rc::new(class_object)));
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expression: &Expression) -> Result<Object, LoxError> {
        match expression {
            Expression::Literal(literal_type) => Ok(Object::from(literal_type.clone())),
            Expression::Grouping(grouped_expression) => {
                let value = self.evaluate(grouped_expression)?;
                Ok(value)
            }
            Expression::Unary(unary_operator, unary_operator_expression) => {
                let value = self.evaluate(unary_operator_expression)?;
                match unary_operator {
                    UnaryOperator::Negate => match value {
                        Object::String(_) => {
                            return Err(LoxError::RuntimeError(
                                "Invalid runtime type for negation operation".to_string(),
                            ));
                        }
                        Object::Number(number) => Ok(Object::Number(-number)),
                        Object::True => Ok(Object::Number(-1.0f64)), // Implicit cast to number
                        Object::False => Ok(Object::Number(0.0f64)), // Implicit cast to number
                        Object::Nil => Ok(Object::Nil),
                        Object::Callable(_) => Err(LoxError::RuntimeError(
                            "Cannot negate callable object".to_string(),
                        )),
                        Object::Class(_) => {
                            Err(LoxError::RuntimeError("Cannot negate class".to_string()))
                        }
                        Object::Instance(_) => {
                            Err(LoxError::RuntimeError("Cannot negate instance".to_string()))
                        }
                    },
                    UnaryOperator::Not => match value {
                        Object::String(_) => Ok(Object::False),
                        Object::Number(_) => Ok(Object::False),
                        Object::True => Ok(Object::False),
                        Object::False => Ok(Object::True),
                        Object::Nil => Ok(Object::True),
                        Object::Callable(_) => Err(LoxError::RuntimeError(
                            "Cannot call logical not on callable object".to_string(),
                        )),
                        Object::Class(_) => Ok(Object::False),
                        Object::Instance(_) => Ok(Object::False),
                    },
                }
            }
            Expression::Binary(lhs_expression, binary_operator, rhs_expression) => {
                let left_value = self.evaluate(lhs_expression)?;
                let right_value = self.evaluate(rhs_expression)?;
                match binary_operator {
                    BinaryOperator::EqualEqual => Ok(Object::from(left_value == right_value)),
                    BinaryOperator::NotEqual => Ok(Object::from(left_value != right_value)),
                    BinaryOperator::Plus => match (left_value, right_value) {
                        (Object::String(str1), Object::String(str2)) => {
                            Ok(Object::String(str1.clone() + &str2))
                        }
                        (Object::Number(num1), Object::Number(num2)) => {
                            Ok(Object::Number(num1 + num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands for +
                                operator"
                                    .to_string(),
                            ))
                        }
                    },
                    // Only for number, true and false type
                    BinaryOperator::LessThan => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            if num1 < num2 {
                                Ok(Object::True)
                            } else {
                                Ok(Object::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to < operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::LessThanEqual => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            if num1 <= num2 {
                                Ok(Object::True)
                            } else {
                                Ok(Object::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to <= operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::GreaterThan => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            if num1 > num2 {
                                Ok(Object::True)
                            } else {
                                Ok(Object::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to > operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::GreaterThanEqual => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            if num1 >= num2 {
                                Ok(Object::True)
                            } else {
                                Ok(Object::False)
                            }
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to >= operator".to_string(),
                            ))
                        }
                    },

                    BinaryOperator::Minus => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            Ok(Object::Number(num1 - num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to - operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::Multiply => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            Ok(Object::Number(num1 * num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to *  operator".to_string(),
                            ))
                        }
                    },
                    BinaryOperator::Divide => match (left_value, right_value) {
                        (Object::Number(num1), Object::Number(num2)) => {
                            Ok(Object::Number(num1 / num2))
                        }
                        _ => {
                            return Err(LoxError::RuntimeError(
                                "Invalid operands given to / operator".to_string(),
                            ))
                        }
                    },
                }
            }
            Expression::Identifier(id, name) => {
                if let Some(distance) = self.local_variable_resolution_table[*id as usize] {
                    if let Some(value) = self.lookup_at_distance(&name, distance.clone()) {
                        return Ok(value.clone());
                    } else {
                        return Err(LoxError::InternalError(format!(
                            "Local variable lookup based on resolution table did not work"
                        )));
                    }
                } else if let Some(value) = self.lookup_global(&name) {
                    return Ok(value.clone());
                } else {
                    return Err(LoxError::RuntimeError(format!(
                        "Use of undeclared variable {}",
                        name
                    )));
                }
            }
            Expression::Assignment(id, name, expr) => {
                let new_value = self.evaluate(expr)?;
                if let Some(distance) = self.local_variable_resolution_table[*id as usize] {
                    if self.update_at_distance(&name, &new_value, distance) {
                        return Ok(Object::Nil);
                    } else {
                        return Err(LoxError::InternalError(format!(
                            "Local variable lookup based on resolution table did not work"
                        )));
                    }
                } else if self.update_global(&name, &new_value) {
                    return Ok(Object::Nil);
                } else {
                    return Err(LoxError::RuntimeError(format!(
                        "Reference to undeclared variable"
                    )));
                }
            }
            Expression::Logical(lhs, op, rhs) => {
                let lhs_value = self.evaluate(lhs)?;
                match op {
                    LogicalOperator::And => {
                        if !is_true_value(&lhs_value) {
                            return Ok(Object::False);
                        }
                        let rhs_value = self.evaluate(rhs)?;
                        if is_true_value(&rhs_value) {
                            return Ok(Object::True);
                        }
                        return Ok(Object::False);
                    }
                    LogicalOperator::Or => {
                        if is_true_value(&lhs_value) {
                            return Ok(Object::True);
                        }
                        let rhs_value = self.evaluate(rhs)?;
                        if is_true_value(&rhs_value) {
                            return Ok(Object::True);
                        }
                        return Ok(Object::False);
                    }
                }
            }
            Expression::Call(call_expression, arguments) => {
                let mut callable = self.evaluate(call_expression)?;
                match &mut callable {
                    Object::Callable(callable_object) => {
                        if callable_object.parameters.len() != arguments.len() {
                            // Check arity
                            return Err(LoxError::RuntimeError(format!(
                                "{} expects {} argument/s, found {}",
                                &callable_object.name,
                                callable_object.parameters.len(),
                                arguments.len()
                            )));
                        }
                        let mut arg_values = Vec::new();
                        for arg_expr in arguments {
                            let value = self.evaluate(arg_expr)?;
                            arg_values.push(value);
                        }
                        let prev = self.current.clone();
                        self.current = Rc::downgrade(&callable_object.environment);
                        for (param, arg_value) in callable_object.parameters.iter().zip(arg_values)
                        {
                            self.update_or_add(param, arg_value);
                        }
                        let return_value: Result<Object, LoxError>;
                        match self.handle_statement(&callable_object.function_block) {
                            Ok(_) => return_value = Ok(Object::Nil),
                            Err(err) => match err {
                                LoxError::Return(value) => {
                                    return_value = Ok(value);
                                }
                                _ => return Err(err),
                            },
                        }
                        self.current = prev;
                        return return_value;
                    }
                    Object::Class(lox_class) => {
                        let new_instance = create_instance(lox_class.clone());
                        match lox_class.methods.get("init") {
                            Some(class_init_callable) => {
                                let instance_init_callable = class_init_callable.clone();
                                instance_init_callable
                                    .environment
                                    .borrow_mut()
                                    .environment
                                    .update_or_add("this", Object::Instance(new_instance.clone()));
                                {
                                    // TODO: Clean me up, reuse Expression::Call code here. It does the same thing
                                    if instance_init_callable.parameters.len() != arguments.len() {
                                        // Check arity
                                        return Err(LoxError::RuntimeError(format!(
                                            "{} expects {} argument/s, found {}",
                                            &instance_init_callable.name,
                                            instance_init_callable.parameters.len(),
                                            arguments.len()
                                        )));
                                    }
                                    let mut arg_values = Vec::new();
                                    for arg_expr in arguments {
                                        let value = self.evaluate(arg_expr)?;
                                        arg_values.push(value);
                                    }
                                    let prev = self.current.clone();
                                    self.current =
                                        Rc::downgrade(&instance_init_callable.environment);
                                    for (param, arg_value) in
                                        instance_init_callable.parameters.iter().zip(arg_values)
                                    {
                                        self.update_or_add(param, arg_value);
                                    }
                                    let _return_value: Result<Object, LoxError>; // TODO: init shouldn't have RV's handle error cases
                                    match self
                                        .handle_statement(&instance_init_callable.function_block)
                                    {
                                        Ok(_) => _return_value = Ok(Object::Nil),
                                        Err(err) => match err {
                                            LoxError::Return(value) => {
                                                _return_value = Ok(value);
                                            }
                                            _ => return Err(err),
                                        },
                                    }
                                    self.current = prev;
                                }
                            }
                            None => {}
                        }
                        return Ok(Object::Instance(new_instance));
                    }
                    _ => {
                        return Err(LoxError::RuntimeError(format!(
                            "{} is not a callable type",
                            callable
                        )))
                    }
                }
            }
            Expression::Get(expression, property_name) => {
                let instance = self.evaluate(expression)?;
                match instance {
                    Object::Instance(class_instance) => {
                        let class_instance_ref_ptr = class_instance.clone();
                        let class_instance = class_instance.borrow();
                        match class_instance.properties.get(property_name) {
                            Some(object) => return Ok(object.clone()),
                            None => {
                                // Did not find property, check if the corresponding class has a method with that name
                                let class = class_instance.class.upgrade().unwrap();
                                let function = class.as_ref().methods.get(property_name);
                                match function {
                                    Some(class_callable_object) => {
                                        let instance_callable_object =
                                            class_callable_object.clone();
                                        instance_callable_object
                                            .environment
                                            .borrow_mut()
                                            .environment
                                            .update_or_add(
                                                "this",
                                                Object::Instance(class_instance_ref_ptr),
                                            );
                                        return Ok(Object::Callable(instance_callable_object));
                                    }
                                    None => {}
                                }
                                return Err(LoxError::RuntimeError(format!(
                                    "Instance of {} does not have property {}",
                                    (*class).name,
                                    property_name
                                )));
                            }
                        }
                    }
                    _ => {
                        return Err(LoxError::RuntimeError(
                            "Get expressions can only be evaluated on instances of classes"
                                .to_string(),
                        ))
                    }
                }
            }
            Expression::Set(lhs, name, rhs) => {
                let mut lhs_object = self.evaluate(lhs)?;
                match &mut lhs_object {
                    Object::Instance(lox_instance) => {
                        let rhs_value = self.evaluate(rhs)?;
                        lox_instance
                            .borrow_mut()
                            .properties
                            .insert(name.clone(), rhs_value);
                        Ok(Object::Nil)
                    }
                    _ => Err(LoxError::RuntimeError(
                        "LHS is not an assignable type".to_string(),
                    )),
                }
            }
            Expression::This(id) => {
                if let Some(distance) = self.local_variable_resolution_table[*id as usize] {
                    if let Some(value) =
                        self.lookup_at_distance(&"this".to_string(), distance.clone())
                    {
                        return Ok(value.clone());
                    }
                }
                return Err(LoxError::InternalError(format!("Could not find \"this\"")));
            }
        }
    }
    fn print_environment_node_helper(&self, node: Weak<RefCell<EnvironmentNode>>, indent: usize) {
        for (name, object) in &node.upgrade().unwrap().borrow().environment.variables {
            print!("{:<width$}", "", width = indent + 2);
            println!("name: {} value={}", name, object);
        }
    }

    fn print_environment_tree_helper(
        &mut self,
        node: Weak<RefCell<EnvironmentNode>>,
        indent: usize,
    ) {
        print!("{:<width$}", "", width = indent);
        println!("{{");
        self.print_environment_node_helper(node.clone(), indent);
        for child in &node.upgrade().unwrap().borrow().children {
            self.print_environment_tree_helper(Rc::downgrade(&child), indent + 4);
        }
        print!("{:<width$}", "", width = indent);
        println!("}}");
    }
    #[allow(dead_code)]
    fn print_environment_tree(&mut self) {
        self.print_environment_tree_helper(Rc::downgrade(&self.global_environment), 0);
    }
    #[allow(dead_code)]
    fn print_current_environment_node(&mut self) {
        self.print_environment_tree_helper(self.current.clone(), 0);
    }

    fn lookup_global(&self, name: &String) -> Option<Object> {
        if let Some(value) = self.global_environment.borrow().environment.lookup(name) {
            Some(value.clone())
        } else {
            None
        }
    }

    fn lookup_at_distance(&self, name: &String, distance: usize) -> Option<Object> {
        let mut current = self.current.clone();
        for _ in 0..distance {
            let rc = current.upgrade().unwrap();
            let current_ref = rc.borrow();
            let parent;
            match &current_ref.parent {
                Some(p) => parent = p.clone(),
                None => break,
            }
            current = parent;
        }
        let rc = current.upgrade().unwrap();
        let current_ref = rc.borrow();
        if let Some(value) = current_ref.environment.lookup(name) {
            Some(value.clone())
        } else {
            None
        }
    }

    fn update_at_distance(&mut self, name: &String, value: &Object, distance: usize) -> bool {
        let mut current = self.current.clone();
        for _ in 0..distance {
            let rc = current.upgrade().unwrap();
            let current_ref = rc.borrow();
            let parent;
            match &current_ref.parent {
                Some(p) => parent = p.clone(),
                None => break,
            }
            current = parent;
        }
        let rc = current.upgrade().unwrap();
        let mut current_ref = rc.borrow_mut();
        if let Some(_) = current_ref.environment.lookup(name) {
            current_ref.environment.update_or_add(&name, value.clone());
            return true;
        } else {
            return false;
        }
    }

    #[allow(dead_code)]
    fn lookup(&self, name: &String) -> Option<Object> {
        let mut current = self.current.clone();
        loop {
            let rc = current.upgrade().unwrap();
            let current_ref = rc.borrow();
            if let Some(value) = current_ref.environment.lookup(name) {
                return Some(value.clone());
            }
            let parent;

            match &current_ref.parent {
                Some(p) => parent = p.clone(),
                None => break,
            }
            current = parent;
        }
        return None;
    }

    fn update_global(&mut self, name: &String, value: &Object) -> bool {
        let mut env = (*self.global_environment).borrow_mut();
        if env.environment.lookup(name).is_some() {
            env.environment.update_or_add(&name, value.clone());
            return true;
        }
        return false;
    }

    #[allow(dead_code)]
    fn update(&mut self, name: &String, value: &Object) -> bool {
        let mut current = self.current.clone();
        loop {
            let rc = current.upgrade().unwrap();
            let mut current_mut_ref = rc.borrow_mut();

            if current_mut_ref.environment.lookup(name).is_some() {
                current_mut_ref
                    .environment
                    .update_or_add(name, value.clone());
                return true;
            }

            let parent;
            match &current_mut_ref.parent {
                Some(p) => parent = p.clone(),
                None => break,
            }
            current = parent;
        }
        return false;
    }

    fn update_or_add(&mut self, name: &String, value: Object) {
        self.current
            .upgrade()
            .unwrap()
            .as_ref()
            .borrow_mut()
            .environment
            .update_or_add(name, value);
    }
}
