use std::{
    env,
    io::{self, Read, Write},
};

mod ast_printer;
mod error;
mod global_handle;
mod interpreter;
mod parser;
mod scanner;
mod tokens;

use error::LoxError;
use global_handle::GlobalHandle;
use parser::Parser;

fn main() -> Result<(), LoxError> {
    let args: Vec<_> = env::args().collect();

    let mut global_handle = GlobalHandle::new();

    match args.len() {
        1 => run_from_console(&mut global_handle)?,
        2 => run_from_file(&args[1], &mut global_handle)?,
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

fn run_from_console(global_handle: &mut GlobalHandle) -> Result<(), LoxError> {
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
        run(&buffer, global_handle)?;
    }
    Ok(())
}

fn run_from_file(filename: &str, global_handle: &mut GlobalHandle) -> Result<(), LoxError> {
    let mut input_file = std::fs::File::open(filename)?;
    let mut file_contents = String::new();
    input_file.read_to_string(&mut file_contents)?;
    run(&file_contents, global_handle)?;
    Ok(())
}

fn run(line: &str, global_handle: &mut GlobalHandle) -> Result<(), LoxError> {
    let tokens = global_handle.scanner.scan_tokens(&(line.as_bytes()))?;
    println!("{:#?}", tokens);
    let mut parser = Parser::new(&tokens);

    let expression = parser.parse()?;

    ast_printer::print_expression(&expression);

    global_handle.interpreter.interpret(&expression)?;

    Ok(())
}
