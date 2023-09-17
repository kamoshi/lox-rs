use std::{env, fs, process};
use crate::error::LoxError;

mod lexer;
mod parser;
mod error;
mod interpreter;
mod repl;


fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        3.. => {
            println!("Usage: lox-rs [script]");
            process::exit(64);
        },
        2 => run_file(&args[1]),
        _ => repl::run_repl(),
    }

    println!()
}


fn run_file(path: &str) {
    let source = fs::read_to_string(path).expect("Couldn't read");
    let _result = run(&source); //.unwrap_or_else(|_| process::exit(65));
}


fn run(source: &str) {
    let tokens = match lexer::tokenize(source) {
        Ok(tokens) => tokens,
        Err(error) => return error.report(),
    };

    let ast = match parser::parse(&tokens) {
        Ok(ast) => ast,
        Err(error) => return error.report(),
    };

    match interpreter::exec(None, &ast) {
        Ok(_) => (),
        Err(err) => err.report(),
    }
}
