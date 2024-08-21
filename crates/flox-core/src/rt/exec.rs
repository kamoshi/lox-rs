use std::collections::HashMap;
use std::rc::Rc;

use super::env::EnvRef;
use super::error::ErrorType;
use super::types::{Callable, LoxFn, LoxType};
use crate::rt::builtin;
use crate::parse::ast::{Expr, Ident, Literal};

fn exec_expr_if(
    env: EnvRef,
    cond: &Expr,
    t: &Expr,
    f: &Option<Box<Expr>>,
) -> Result<LoxType, ErrorType> {
    let cond = eval_expr(env.clone(), cond)?;

    match (cond.is_truthy(), f) {
        (true, _) => eval_expr(env, t),
        (_, Some(e)) => eval_expr(env, e),
        _ => Ok(LoxType::Nil),
    }
}

pub fn eval_expr(env: EnvRef, expr: &Expr) -> Result<LoxType, ErrorType> {
    match expr {
        Expr::Literal(literal) => Ok(eval_expr_literal(literal)),
        Expr::Unary(op, expr) => eval_expr_unary(env, op, expr),
        Expr::Binary(l, op, r) => eval_expr_binary(env, l, op, r),
        Expr::Grouping(expr) => eval_expr_grouping(env, expr),
        Expr::Variable(ident) => eval_expr_variable(env, ident),
        Expr::Assign(ident, expr) => eval_expr_assign(env, ident, expr),
        Expr::Call(callee, arg) => eval_expr_call(env, callee, arg),
        Expr::Lambda(ident, block) => eval_lambda(env, ident, block),
        Expr::Array(exprs) => eval_expr_array(env, exprs),
        Expr::Tuple(exprs) => eval_expr_tuple(env, exprs),
        Expr::If(cond, t, f) => exec_expr_if(env, cond, t, f),
        Expr::Block(exprs) => exec_expr_block(env, exprs),
        Expr::Let(ident, expr) => exec_expr_let(env, ident, expr),
        Expr::While(cond, body) => eval_expr_while(env, cond, body),
        Expr::Return(expr) => eval_expr_return(env, expr.as_deref()),
        Expr::Data(ident, elems) => eval_expr_data(env, ident, elems),
        Expr::Match(expr, cases) => eval_expr_match(env, expr, cases),
        Expr::Index(expr, ident) => eval_expr_index(env, expr, ident),
        Expr::Constructor(t, v, fields) => eval_expr_constructor(env, *t, *v, fields),
    }
}

fn eval_expr_constructor(env: EnvRef, t: usize, v: usize, fields: &[String]) -> Result<LoxType, ErrorType> {
    let test = env.borrow();
    let mut vals = vec![];
    for field in fields {
        vals.push(test.get(field)?);
    }
    Ok(LoxType::Data(t, v, vals.into()))
}

fn eval_expr_index(env: EnvRef, expr: &Expr, ident: &Ident) -> Result<LoxType, ErrorType> {
    match eval_expr(env, expr)? {
        LoxType::Type(_, assoc) => Ok(assoc.get(&ident.0).unwrap().clone()),
        _ => todo!()
    }
}

fn eval_expr_match(env: EnvRef, expr: &Expr, cases: &[(Expr, Expr, Box<[String]>)]) -> Result<LoxType, ErrorType> {
    let id = eval_expr(env.clone(), expr)?;
    let ty = id.get_type();
    let vals = match id {
        LoxType::Data(_, _, vals) => vals,
        _ => Box::new([]),
    };

    for (cond, then, idents) in cases {
        let test = eval_expr(env.clone(), cond)?.get_type();

        if ty == test {
            for (k, v) in vals.iter().zip(idents.iter()) {
                env.borrow_mut().define(v, k);
            }
            return eval_expr(env, then);
        }
    }

    Ok(LoxType::Nil)
}

fn eval_expr_data(env: EnvRef, ident: &Ident, elems: &[(Ident, Box<[String]>)]) -> Result<LoxType, ErrorType> {
    let ty = env.borrow().new_type(ident.0.clone());
    let mut assoc = vec![];

    for (i, (name, fields)) in elems.iter().enumerate() {
        if fields.len() == 0 {
            assoc.push((name.0.clone(), LoxType::Callable(Callable::Constructor(ty, i, None))));
            continue;
        }
        let mut args = fields.iter().rev();
        let last = args.next().unwrap();
        let func = args.fold(
            Expr::Lambda(Ident(last.into()), Expr::Constructor(ty, i, fields.clone()).into()),
            |acc, next| Expr::Lambda(Ident(next.into()), acc.into())
        );
        if let LoxType::Callable(Callable::Function(f)) = eval_expr(env.clone(), &func)? {
            assoc.push((name.0.clone(), LoxType::Callable(Callable::Constructor(ty, i, Some(f)))))
        }
    }

    Ok(LoxType::Type(ty, HashMap::from_iter(assoc)))
}

fn eval_expr_return(env: EnvRef, expr: Option<&Expr>) -> Result<LoxType, ErrorType> {
    let res = match expr {
        Some(expr) => eval_expr(env, expr)?,
        None => LoxType::Nil,
    };

    Err(ErrorType::Return(res))
}

fn eval_expr_while(env: EnvRef, cond: &Expr, body: &Expr) -> Result<LoxType, ErrorType> {
    let mut res = LoxType::Nil;
    while eval_expr(env.clone(), cond)?.is_truthy() {
        res = eval_expr(env.clone(), body)?;
    }
    Ok(res)
}

fn exec_expr_let(env: EnvRef, ident: &Ident, expr: &Expr) -> Result<LoxType, ErrorType> {
    let expr = eval_expr(env.clone(), expr)?;
    env.borrow_mut().define(&ident.0, &expr);
    Ok(expr)
}

fn exec_expr_block(env: EnvRef, exprs: &[Expr]) -> Result<LoxType, ErrorType> {
    let mut ret = LoxType::Nil;

    for expr in exprs {
        ret = eval_expr(env.clone(), expr)?;
    }

    Ok(ret)
}

fn eval_expr_tuple(env: EnvRef, exprs: &[Expr]) -> Result<LoxType, ErrorType> {
    let mut arr = vec![];
    for expr in exprs {
        arr.push(eval_expr(env.clone(), expr)?);
    }
    Ok(LoxType::Tuple(arr.into()))
}

fn eval_expr_array(env: EnvRef, exprs: &[Expr]) -> Result<LoxType, ErrorType> {
    let mut arr = vec![];
    for expr in exprs {
        arr.push(eval_expr(env.clone(), expr)?);
    }
    Ok(LoxType::Array(arr))
}

fn eval_lambda(env: EnvRef, param: &Ident, body: &Expr) -> Result<LoxType, ErrorType> {
    let x = Rc::new(LoxFn::new(param.0.clone(), body.clone().into(), env.clone()));
    Ok(LoxType::Callable(Callable::new(x)))
}

fn eval_expr_call(env: EnvRef, callee: &Expr, arg: &Expr) -> Result<LoxType, ErrorType> {
    let callee = eval_expr(env.clone(), callee)?;
    let arg = eval_expr(env.clone(), arg)?;

    match callee {
        LoxType::Callable(c) => c.call(&arg),
        _ => {
            println!("Can't call {}", callee);
            Err(ErrorType::TypeMismatch("Can't call this value"))
        },
    }
}

fn eval_expr_variable(env: EnvRef, ident: &Ident) -> Result<LoxType, ErrorType> {
    env.borrow().get(&ident.0)
}

fn eval_expr_assign(env: EnvRef, ident: &Ident, expr: &Expr) -> Result<LoxType, ErrorType> {
    let value = eval_expr(env.clone(), expr)?;

    env.borrow_mut().set(&ident.0, &value)?;
    Ok(value)
}

fn eval_expr_literal(literal: &Literal) -> LoxType {
    use LoxType::*;
    match literal {
        Literal::Num(n) => Number(*n),
        Literal::Str(s) => String(s.to_owned()),
        Literal::True => Boolean(true),
        Literal::False => Boolean(false),
        Literal::Nil => Nil,
    }
}

fn eval_expr_unary(env: EnvRef, op: &str, expr: &Expr) -> Result<LoxType, ErrorType> {
    let value = eval_expr(env, expr)?;

    use LoxType::*;
    match op {
        "!" => Ok(LoxType::Boolean(!value.is_truthy())),
        "-" => match value {
            Nil => Err(ErrorType::TypeMismatch("Can't negate a nil value")),
            Boolean(_) => Err(ErrorType::TypeMismatch("Can't negate a boolean value")),
            Number(f) => Ok(Number(-f)),
            String(_) => Err(ErrorType::TypeMismatch("Can't negate a string value")),
            Callable(_) => Err(ErrorType::TypeMismatch("Can't negate a function value")),
            Array(_) => Err(ErrorType::TypeMismatch("Can't negate a function value")),
            _ => todo!(),
        },
        _ => Err(ErrorType::TypeMismatch("Missing operator")),
    }
}

fn eval_expr_binary(env: EnvRef, l: &Expr, op: &str, r: &Expr) -> Result<LoxType, ErrorType> {
    let l = eval_expr(env.clone(), l)?;
    let r = eval_expr(env.clone(), r)?;

    match op {
        "==" => builtin::eq(env, l, r),
        "!=" => builtin::neq(env, l, r),
        "<" => builtin::lt(env, l, r),
        "<=" => builtin::lte(env, l, r),
        ">" => builtin::cmp_gt(env, l, r),
        ">=" => builtin::cmp_gte(env, l, r),
        "+" => builtin::math_add(env, l, r),
        "-" => builtin::math_sub(env, l, r),
        "*" => builtin::math_mul(env, l, r),
        "/" => builtin::math_div(env, l, r),
        "||" => builtin::logic_or(env, l, r),
        "&&" => builtin::logic_and(env, l, r),
        "$" => builtin::apply(env, l, r),
        "|>" => builtin::pipe(env, l, r),
        _ => Err(ErrorType::TypeMismatch("Missing operator")),
    }
}

fn eval_expr_grouping(env: EnvRef, expr: &Expr) -> Result<LoxType, ErrorType> {
    eval_expr(env, expr)
}
