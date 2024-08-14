use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::error::LoxError;
use crate::interpreter::{
    self,
    env::{Env, EnvRef},
};
use crate::lex;
use crate::parser;

pub(crate) fn run_repl(lex: bool, parse: bool) {
    let env = Env::new_ref();
    interpreter::native::populate(env.clone());

    let mut rl = DefaultEditor::new().unwrap();

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                if lex {
                    let _ = lex::tokenize(&line).map(|x| println!("{x:?}"));
                } else if parse {
                    match lex::tokenize(&line).map(|tokens| parser::parse(&tokens)) {
                        Ok(Ok(res)) => println!("{:#?}", res),
                        _ => todo!(),
                    };
                } else {
                    eval(env.clone(), &line);
                }
                rl.add_history_entry(&line).unwrap();
                println!();
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

fn eval(env: EnvRef, source: &str) {
    // First try parsing expressions and then try statements
    let tokens = match lex::tokenize(source) {
        Ok(tokens) => tokens,
        Err(error) => return error.report_rich(source),
    };

    let ast = parser::parse(&tokens);

    let result = match ast {
        Ok((_, ast)) => interpreter::eval_expr(env, &ast).map(|res| print!("{res}")),
        Err(error) => return error.report_rich(source),
    };

    match result {
        Ok(()) => (),
        Err(err) => err.report(),
    }
}
