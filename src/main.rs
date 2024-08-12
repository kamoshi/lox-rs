use std::{env, fs, process};

use clap::Parser;

use crate::error::LoxError;
use crate::interpreter::{env::Env, native::populate};

mod error;
mod interpreter;
mod lexer;
mod parser;
mod repl;

#[derive(Parser)]
struct Args {
    /// Optional script file to run
    script: Option<String>,

    /// Run lexer in repl
    #[arg(long)]
    lex: bool,

    /// Run parser in repl
    #[arg(long)]
    parse: bool,
}

fn main() {
    let args = Args::parse();

    match args.script {
        Some(name) => run_file(&name),
        None => {
            repl::run_repl(args.lex, args.parse);
        }
    }
}

fn run_file(path: &str) {
    let source = fs::read_to_string(path).expect("Couldn't read");
    let _result = run(&source); //.unwrap_or_else(|_| process::exit(65));
}

fn run(source: &str) {
    let tokens = match lexer::tokenize(source) {
        Ok(tokens) => tokens,
        Err(error) => return error.report_rich(source),
    };

    let ast = match parser::parse(&tokens) {
        Ok(ast) => ast,
        Err(error) => return error.report_rich(source),
    };

    let env = Env::new_ref();
    populate(env.clone());

    match interpreter::exec(Some(env), &ast) {
        Ok(_) => (),
        Err(err) => err.report(),
    }
}
