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
    // First try parsing expressions and then try statements
    let _ = lexer::tokenize(source)
        .map_err(LoxError::wrap)
        .and_then(|tokens|
            // expressions
            parser::parse_expr(&tokens)
                .map_err(LoxError::wrap)
                .map(|expr| ReplMode::Expr(expr))
            // statements
            .or_else(|_| parser::parse(&tokens)
                .map_err(LoxError::wrap)
                .map(|stmt| ReplMode::Stmt(stmt))
            )
        )
        // run interpreter for AST
        .and_then(|ast| match ast {
            ReplMode::Stmt(stmt) => interpreter::exec(&stmt)
                .map_err(LoxError::wrap),
            ReplMode::Expr(expr) => interpreter::eval_expr(&expr)
                .map_err(LoxError::wrap)
                .map(|res| print!("{res}")),
        })
        // report any errors
        .map_err(|err| err.report());
}
