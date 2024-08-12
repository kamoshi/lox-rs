use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use crate::error::LoxError;
use crate::interpreter::{
    self,
    env::{Env, EnvRef},
};
use crate::lexer;
use crate::parser::{self, ast};

enum ReplMode {
    Stmt(Vec<ast::Stmt>),
    Expr(ast::Expr),
}

impl From<Vec<ast::Stmt>> for ReplMode {
    fn from(value: Vec<ast::Stmt>) -> Self {
        Self::Stmt(value)
    }
}

impl From<ast::Expr> for ReplMode {
    fn from(value: ast::Expr) -> Self {
        Self::Expr(value)
    }
}

pub(crate) fn run_repl(lex: bool, parse: bool) {
    let env = Env::new_ref();
    interpreter::native::populate(env.clone());

    let mut rl = DefaultEditor::new().unwrap();

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                if lex {
                    let _ = lexer::tokenize(&line).map(|x| println!("{x:?}"));
                } else if parse {
                    match lexer::tokenize(&line).map(|tokens| parser::parse_expr(&tokens)) {
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
    let tokens = match lexer::tokenize(source) {
        Ok(tokens) => tokens,
        Err(error) => return error.report_rich(source),
    };

    let ast = parser::parse_expr(&tokens)
        .map(ReplMode::from)
        .or_else(|_| parser::parse(&tokens).map(ReplMode::from));

    let ast = match ast {
        Ok(ast) => ast,
        Err(error) => return error.report_rich(source),
    };

    let result = match ast {
        ReplMode::Stmt(stmt) => interpreter::exec(Some(env), &stmt),
        ReplMode::Expr(expr) => interpreter::eval_expr(env, &expr).map(|res| print!("{res}")),
    };

    match result {
        Ok(()) => (),
        Err(err) => err.report(),
    }
}
