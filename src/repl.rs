use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use flox_core::{error::FloxError, parse::parser::Context};
use crate::rt::{
    self,
    env::{Env, EnvRef},
};
use crate::lex;
use crate::parser;

pub(crate) fn run_repl(lex: bool, parse: bool) {
    let env = Env::new_ref();
    flox_core::rt::native::populate(env.clone());

    let mut rl = DefaultEditor::new().unwrap();
    let ctx = Context::default();

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                if lex {
                    let _ = lex::tokenize(&line).map(|x| println!("{x:?}"));
                } else if parse {
                    let res = match lex::tokenize(&line).map(|tokens| parser::parse(&ctx, &tokens)) {
                        Ok(Ok(res)) => format!("{:#?}", res),
                        Err(err) => err.report_rich(&line),
                        Ok(Err(err)) => err.report_rich(&line),
                    };
                    println!("{}", res)
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
        Err(error) => return eprintln!("{}", error.report_rich(source)),
    };

    let ast = parser::parse(&Context::default(), &tokens);

    let result = match ast {
        Ok((_, ast)) => flox_core::rt::eval_expr(env, &ast).map(|res| print!("{res}")),
        Err(error) => return eprintln!("{}", error.report_rich(source)),
    };

    match result {
        Ok(()) => (),
        Err(err) => eprintln!("{}", err.report()),
    }
}
