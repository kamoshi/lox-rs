use crate::parser::ast::{Stmt, Expr, Literal, OpUnary, OpBinary};

use super::types::LoxType;
use super::env::Env;
use super::error::ErrorType;


pub fn exec(stmts: &[Stmt]) -> Result<(), ErrorType> {
    let mut env = Env::new();
    for stmt in stmts {
        exec_stmt(stmt)?;
    };
    Ok(())
}

fn exec_stmt(stmt: &Stmt) -> Result<(), ErrorType> {
    match stmt {
        Stmt::Var(_, _)         => todo!(),
        Stmt::Expression(expr)  => exec_stmt_expr(expr)?,
        Stmt::Print(expr)       => exec_stmt_prnt(expr)?,
    };
    Ok(())
}

fn exec_stmt_expr(expr: &Expr) -> Result<(), ErrorType> {
    eval_expr(expr)?;
    Ok(())
}

fn exec_stmt_prnt(expr: &Expr) -> Result<(), ErrorType> {
    let res = eval_expr(expr)?;
    print!("{res}");
    Ok(())
}


pub fn eval_expr(expr: &Expr) -> Result<LoxType, ErrorType> {
    match expr {
        Expr::Literal(literal)  => Ok(eval_expr_literal(literal)),
        Expr::Unary(op, expr)   => eval_expr_unary(op, expr),
        Expr::Binary(l, op, r)  => eval_expr_binary(l, op, r),
        Expr::Grouping(expr)    => eval_expr_grouping(expr),
        Expr::Variable(_) => todo!(),
        Expr::Assign(_, _) => todo!(),
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

fn eval_expr_unary(op: &OpUnary, expr: &Box<Expr>) -> Result<LoxType, ErrorType> {
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
            Nil         => Err(ErrorType::TypeMismatch("Can't negate a nil value")),
            Boolean(_)  => Err(ErrorType::TypeMismatch("Can't negate a boolean value")),
            Number(f)   => Ok(Number(-f)),
            String(_)   => Err(ErrorType::TypeMismatch("Can't negate a string value")),
        },
    }
}

fn eval_expr_binary(l: &Box<Expr>, op: &OpBinary, r: &Box<Expr>) -> Result<LoxType, ErrorType> {
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
            _ => Err(ErrorType::TypeMismatch("Can't compare non numbers")),
        },
        LessEqual => match (l, r) {
            (Number(l), Number(r)) => Ok(Boolean(l <= r)),
            _ => Err(ErrorType::TypeMismatch("Can't compare non numbers")),
        },
        Greater => match (l, r) {
            (Number(l), Number(r)) => Ok(Boolean(l > r)),
            _ => Err(ErrorType::TypeMismatch("Can't compare non numbers")),
        },
        GreaterEqual => match (l, r) {
            (Number(l), Number(r)) => Ok(Boolean(l >= r)),
            _ => Err(ErrorType::TypeMismatch("Can't compare non numbers")),
        },
        Add => match (l, r) {
            (Number(l), Number(r)) => Ok(Number(l + r)),
            (String(l), String(r)) => Ok(String(format!("{l}{r}"))),
            _ => Err(ErrorType::TypeMismatch("Can only add two numbers or two strings")),
        },
        Sub => match (l, r) {
            (Number(l), Number(r)) => Ok(Number(l - r)),
            _ => Err(ErrorType::TypeMismatch("Can't sub non numbers")),
        },
        Mul => match (l, r) {
            (Number(l), Number(r)) => Ok(Number(l * r)),
            _ => Err(ErrorType::TypeMismatch("Can't mul non numbers")),
        },
        Div => match (l, r) {
            (Number(l), Number(r)) => Ok(Number(l / r)),
            _ => Err(ErrorType::TypeMismatch("Can't div non numbers")),
        },
    }
}

fn eval_expr_grouping(expr: &Box<Expr>) -> Result<LoxType, ErrorType> {
    eval_expr(expr)
}
