use std::fmt::Display;
use crate::{parser::{expr::{Expr, Literal, OpUnary, OpBinary}, stmt::Stmt}, error::LoxError};


#[derive(PartialEq)]
pub enum LoxType {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LoxType::*;
        match self {
            Nil         => write!(f, "nil"),
            Boolean(b)  => write!(f, "{b}"),
            Number(n)   => write!(f, "{n}"),
            String(s)   => write!(f, "{s}"),
        }
    }
}


pub enum Error {
    TypeMismatch(&'static str)
}

impl LoxError for Error {
    fn report(&self) {
        let message = match self {
            Error::TypeMismatch(s) => format!("Type mismatch: {s}"),
        };

        eprintln!("{message}");
    }
}


pub(crate) fn exec_stmt(stmt: &Stmt) -> Result<(), Error> {
    match stmt {
        Stmt::Expression(expr) => run_stmt_expr(expr)?,
        Stmt::Print(expr)      => run_stmt_prnt(expr)?,
    };
    Ok(())
}

fn run_stmt_expr(expr: &Expr) -> Result<(), Error> {
    eval_expr(expr)?;
    Ok(())
}

fn run_stmt_prnt(expr: &Expr) -> Result<(), Error> {
    let res = eval_expr(expr)?;
    print!("{res}");
    Ok(())
}


pub(crate) fn eval_expr(expr: &Expr) -> Result<LoxType, Error> {
    match expr {
        Expr::Literal(literal)  => Ok(eval_expr_literal(literal)),
        Expr::Unary(op, expr)   => eval_expr_unary(op, expr),
        Expr::Binary(l, op, r)  => eval_expr_binary(l, op, r),
        Expr::Grouping(expr)    => eval_expr_grouping(expr),
    }
}

fn eval_expr_literal(literal: &Literal) -> LoxType {
    use LoxType::*;
    match literal {
        Literal::Num(n) => Number(*n),
        Literal::Str(s) => String(s.to_owned()),
        Literal::True   => Boolean(true),
        Literal::False  => Boolean(false),
        Literal::Nil    => Nil,
    }
}

fn eval_expr_unary(op: &OpUnary, expr: &Box<Expr>) -> Result<LoxType, Error> {
    let value = eval_expr(expr)?;

    use LoxType::*;
    match op {
        OpUnary::Not => match value {
            Nil         => Ok(Boolean(true)),
            Boolean(b)  => Ok(Boolean(!b)),
            Number(_)   => Ok(Boolean(false)),
            String(_)   => Ok(Boolean(false)),
        },
        OpUnary::Neg => match value {
            Nil         => Err(Error::TypeMismatch("Can't negate a nil value")),
            Boolean(_)  => Err(Error::TypeMismatch("Can't negate a boolean value")),
            Number(f)   => Ok(Number(-f)),
            String(_)   => Err(Error::TypeMismatch("Can't negate a string value")),
        },
    }
}

fn eval_expr_binary(l: &Box<Expr>, op: &OpBinary, r: &Box<Expr>) -> Result<LoxType, Error> {
    let l = eval_expr(l)?;
    let r = eval_expr(r)?;

    use OpBinary::*;
    use LoxType::*;
    match op {
        Equal => match (l, r) {
            (Nil, Nil)  => Ok(Boolean(true)),
            (l, r)      => Ok(Boolean(l == r)),
        },
        NotEqual => match (l, r) {
            (Nil, Nil)  => Ok(Boolean(false)),
            (l, r)      => Ok(Boolean(l != r)),
        },
        Less => match (l, r) {
            (Number(l), Number(r)) => Ok(Boolean(l < r)),
            _ => Err(Error::TypeMismatch("Can't compare non numbers")),
        },
        LessEqual => match (l, r) {
            (Number(l), Number(r)) => Ok(Boolean(l <= r)),
            _ => Err(Error::TypeMismatch("Can't compare non numbers")),
        },
        Greater => match (l, r) {
            (Number(l), Number(r)) => Ok(Boolean(l > r)),
            _ => Err(Error::TypeMismatch("Can't compare non numbers")),
        },
        GreaterEqual => match (l, r) {
            (Number(l), Number(r)) => Ok(Boolean(l >= r)),
            _ => Err(Error::TypeMismatch("Can't compare non numbers")),
        },
        Add => match (l, r) {
            (Number(l), Number(r)) => Ok(Number(l + r)),
            (String(l), String(r)) => Ok(String(format!("{l}{r}"))),
            _ => Err(Error::TypeMismatch("Can only add two numbers or two strings")),
        },
        Sub => match (l, r) {
            (Number(l), Number(r)) => Ok(Number(l - r)),
            _ => Err(Error::TypeMismatch("Can't sub non numbers")),
        },
        Mul => match (l, r) {
            (Number(l), Number(r)) => Ok(Number(l * r)),
            _ => Err(Error::TypeMismatch("Can't mul non numbers")),
        },
        Div => match (l, r) {
            (Number(l), Number(r)) => Ok(Number(l / r)),
            _ => Err(Error::TypeMismatch("Can't div non numbers")),
        },
    }
}

fn eval_expr_grouping(expr: &Box<Expr>) -> Result<LoxType, Error> {
    eval_expr(expr)
}
