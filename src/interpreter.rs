use std::fmt::Display;
use crate::parser::expr::{Expr, Literal, OpUnary, OpBinary};


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
            Nil         => write!(f, "Nil"),
            Boolean(b)  => write!(f, "{b}"),
            Number(n)   => write!(f, "{n}"),
            String(s)   => write!(f, "{s}"),
        }
    }
}


pub(crate) fn eval_expr(expr: &Expr) -> LoxType {
    match expr {
        Expr::Literal(literal)  => eval_expr_literal(literal),
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

fn eval_expr_unary(op: &OpUnary, expr: &Box<Expr>) -> LoxType {
    let value = eval_expr(expr);

    use LoxType::*;
    match op {
        OpUnary::Not => match value {
            Nil         => Boolean(true),
            Boolean(b)  => Boolean(!b),
            Number(_)   => Boolean(false),
            String(_)   => Boolean(false),
        },
        OpUnary::Neg => match value {
            Nil         => panic!(),
            Boolean(_)  => panic!(),
            Number(f)   => Number(-f),
            String(_)   => panic!(),
        },
    }
}

fn eval_expr_binary(l: &Box<Expr>, op: &OpBinary, r: &Box<Expr>) -> LoxType {
    let l = eval_expr(l);
    let r = eval_expr(r);

    use OpBinary::*;
    use LoxType::*;
    match op {
        Equal => match (l, r) {
            (Number(l), Number(r)) => Boolean(l == r),
            _ => panic!(),
        },
        NotEqual => match (l, r) {
            (Number(l), Number(r)) => Boolean(l != r),
            _ => panic!(),
        },
        Less => match (l, r) {
            (Number(l), Number(r)) => Boolean(l < r),
            _ => panic!(),
        },
        LessEqual => match (l, r) {
            (Number(l), Number(r)) => Boolean(l <= r),
            _ => panic!(),
        },
        Greater => match (l, r) {
            (Number(l), Number(r)) => Boolean(l > r),
            _ => panic!(),
        },
        GreaterEqual => match (l, r) {
            (Number(l), Number(r)) => Boolean(l >= r),
            _ => panic!(),
        },
        Add => match (l, r) {
            (Number(l), Number(r)) => Number(l + r),
            (String(l), String(r)) => String(format!("{l}{r}")),
            _ => panic!(),
        },
        Sub => match (l, r) {
            (Number(l), Number(r)) => Number(l - r),
            _ => panic!(),
        },
        Mul => match (l, r) {
            (Number(l), Number(r)) => Number(l * r),
            _ => panic!(),
        },
        Div => match (l, r) {
            (Number(l), Number(r)) => Number(l / r),
            _ => panic!(),
        },
    }
}

fn eval_expr_grouping(expr: &Box<Expr>) -> LoxType {
    eval_expr(expr)
}
