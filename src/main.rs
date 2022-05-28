use std::{
    env,
    io::{self, Read, Write},
};
mod error;
use error::LoxError;

fn print_usage() {
    let help_message = r#"
    "Usage: 1) lox_rust
            2) lox_rust <PATH_TO_LOX_FILE>
    "#;
    println!("{}", help_message);
}

fn run(line: &str) {
    println!("Running line {}", line);
}

fn run_from_console() -> Result<(), LoxError> {
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
        run(&buffer);
    }
    Ok(())
}

fn run_from_file(filename: &str) -> Result<(), LoxError> {
    let mut input_file = std::fs::File::open(filename)?;
    let mut file_contents = String::new();
    input_file.read_to_string(&mut file_contents)?;

    todo!("Complete me");
}

fn main() -> Result<(), LoxError> {
    let args: Vec<_> = env::args().collect();

    match args.len() {
        1 => run_from_console()?,
        2 => run_from_file(&args[2])?,
        _ => print_usage(),
    };

    Ok(())
}
