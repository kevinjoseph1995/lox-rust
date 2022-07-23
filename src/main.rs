use std::{
    env,
    io::{self, Read, Write},
};

mod ast_printer;
mod error;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod tokens;

use error::LoxError;
use interpreter::Interpreter;
use parser::Parser;

fn main() -> Result<(), LoxError> {
    let args: Vec<_> = env::args().collect();

    let mut interpreter = Interpreter::new();

    match args.len() {
        1 => run_from_console(&mut interpreter)?,
        2 => run_from_file(&args[1], &mut interpreter)?,
        _ => print_usage(),
    };

    Ok(())
}

fn print_usage() {
    let help_message = r#"
    "Usage: 1) lox_rust
            2) lox_rust <PATH_TO_LOX_FILE>
    "#;
    println!("{}", help_message);
}

fn run_from_console(interpreter: &mut Interpreter) -> Result<(), LoxError> {
    let mut buffer = String::new();
    loop {
        buffer.clear();
        print!(">> ");
        io::stdout().flush()?;
        std::io::stdin().read_line(&mut buffer)?;
        if buffer == "quit\n" {
            break;
        } else if buffer == "clear\n" {
            print!("{}[2J", 27 as char);
            print!("{esc}[2J{esc}[1;1H", esc = 27 as char);
            continue;
        }
        run(&buffer, interpreter);
    }
    Ok(())
}

fn run_from_file(filename: &str, interpreter: &mut Interpreter) -> Result<(), LoxError> {
    let mut input_file = std::fs::File::open(filename)?;
    let mut file_contents = String::new();
    input_file.read_to_string(&mut file_contents)?;
    run(&file_contents, interpreter);
    Ok(())
}

fn run(line: &str, interpreter: &mut Interpreter) {
    match run_wrapper(&line, interpreter) {
        Err(err) => {
            println!("[\x1b[1;31mRuntime Error\x1b[0m] {}", err);
        }
        Ok(_) => {}
    }
}

fn run_wrapper(line: &str, interpreter: &mut Interpreter) -> Result<(), LoxError> {
    // Scan
    let tokens = scanner::scan_tokens(&(line.as_bytes()))?;
    // println!("{:#?}", tokens);
    let mut parser = Parser::new(tokens);
    let program = parser.parse()?;

    ast_printer::visualize_program_ast(&program); //  Uncomment me to visualize the AST

    interpreter.interpret(program)?;

    Ok(())
}
