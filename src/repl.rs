use std::io::{self, Write};
use crate::{lexer, error::LoxError, parser, interpreter};


enum ReplMode {
    Stmt(Vec<parser::stmt::Stmt>),
    Expr(parser::expr::Expr),
}

pub(crate) fn run_repl() {
    let mut buffer = String::new();

    loop {
        buffer.clear();
        let read = match read(&mut buffer) {
            Ok(0) => break,
            Err(err) => return eprintln!("{err}"),
            Ok(_) => &buffer,
        };

        eval(&read);
        println!("");
    }
}


fn read(buffer: &mut String) -> Result<usize, io::Error> {
    print!("> ");
    let _ = io::stdout().flush();
    io::stdin().read_line(buffer)
}

fn eval(source: &str) {
    let tokens = match lexer::tokenize(source) {
        Ok(tokens) => tokens,
        Err(err) => return err.report(),
    };
    // First try parsing expressions and then try statements
    let ast = parser::parse_expr(&tokens).map(|expr| ReplMode::Expr(expr))
        .or_else(|_| parser::parse(&tokens).map(|stmt| ReplMode::Stmt(stmt)));
    let ast = match ast {
        Ok(ast) => ast,
        Err(err) => return err.report(),
    };

    match ast {
        ReplMode::Stmt(ast) => {
            for stmt in ast {
                match interpreter::exec_stmt(&stmt) {
                    Ok(_) => (),
                    Err(err) => return err.report(),
                };
            }
        },
        ReplMode::Expr(expr) => {
            match interpreter::eval_expr(&expr) {
                Ok(res) => print!("{res}"),
                Err(err) => return err.report(),
            };
        },
    };
}
