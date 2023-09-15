use std::io::{self, Write};
use crate::lexer;
use crate::parser::{self, ast};
use crate::interpreter;
use crate::error::LoxError;


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
    let tokens = match lexer::tokenize(source) {
        Ok(tokens) => tokens,
        Err(error) => return error.report(),
    };

    let ast = parser::parse_expr(&tokens).map(ReplMode::from)
        .or_else(|_| parser::parse(&tokens).map(ReplMode::from));

    let ast = match ast {
        Ok(ast) => ast,
        Err(error) => return error.report(),
    };

    let result = match ast {
        ReplMode::Stmt(stmt) => interpreter::exec(&stmt),
        ReplMode::Expr(expr) => interpreter::eval_expr(&expr).map(|res| print!("{res}")),
    };

    match result {
        Ok(()) => (),
        Err(err) => err.report(),
    }
}
