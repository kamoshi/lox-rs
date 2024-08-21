use clap::Parser;
use flox_core::{
    error::LoxError,
    lex,
    parse::parser::{self, Context},
    rt::{self, env::Env, native::populate, types::LoxType},
};
use std::fs;

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
    let result = run(&source); //.unwrap_or_else(|_| process::exit(65));

    match result {
        Ok(res) => println!("{res}"),
        Err(_) => (),
    }
}

fn run(source: &str) -> Result<LoxType, ()> {
    let tokens = match lex::tokenize(source) {
        Ok(tokens) => tokens,
        Err(error) => return Err(error.report_rich(source)),
    };

    let ast = match parser::parse(&Context::default(), &tokens) {
        Ok((_, ast)) => ast,
        Err(error) => return Err(error.report_rich(source)),
    };

    let env = Env::new_ref();
    populate(env.clone());

    match rt::eval_expr(env, &ast) {
        Ok(res) => Ok(res),
        Err(e) => Err(e.report_rich(source)),
    }
}
