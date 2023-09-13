use std::{env, fs, io::{self, Write}, process};
use crate::error::LoxError;

mod lexer;
mod parser;
mod error;
mod interpreter;


fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        3.. => {
            println!("Usage: lox-rs [script]");
            process::exit(64);
        },
        2 => run_file(&args[1]),
        _ => run_prompt(),
    }

    println!()
}


fn run_file(path: &str) {
    let source = fs::read_to_string(path).expect("Couldn't read");
    let result = run(&source); //.unwrap_or_else(|_| process::exit(65));
}

fn run_prompt() {
    let mut buffer = String::new();

    loop {
        print!("> ");
        let _ = io::stdout().flush();
        match io::stdin().read_line(&mut buffer) {
            Ok(0) => break,
            Ok(_) => {
                let _ = run(&buffer);
            },
            Err(err) => {
                println!("{err}");
                break;
            },
        };
        buffer.clear();
    }
}

fn run(source: &str) {
    let tokens = match lexer::scan_tokens(source) {
        Ok(tokens) => tokens,
        Err(err) => {
            err.report();
            return;
        },
    };
    let ast = match parser::parse(&tokens) {
        Ok(ast) => ast,
        Err(err) => {
            err.report();
            return;
        },
    };
    for stmt in ast {
        match interpreter::run_stmt(&stmt) {
            Ok(_) => (),
            Err(err) => {
                err.report();
                return
            },
        }
    }
}
