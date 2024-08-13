use std::rc::Rc;

use super::env::{Env, EnvRef};
use super::error::ErrorType;
use super::types::{LoxFn, LoxType};
use crate::interpreter::builtin;
use crate::parser::ast::{Expr, Ident, Literal};

// pub fn exec(g_env: Option<EnvRef>, stmts: &[Stmt]) -> Result<(), ErrorType> {
//     let mut env = match g_env {
//         Some(env) => env,
//         None => Env::new_ref(),
//     };
//
//     for stmt in stmts {
//         // lexical scope fix
//         // scheme style!
//         if matches!(stmt, Stmt::Var(..)) {
//             env = Env::wrap(env);
//         }
//
//         exec_stmt(env.clone(), stmt)?;
//     }
//
//     Ok(())
// }

// fn exec_stmt(env: EnvRef, stmt: &Stmt) -> Result<(), ErrorType> {
//     match stmt {
//         Stmt::Var(ident, expr) => exec_stmt_var(env, ident, expr)?,
//         Stmt::Expression(expr) => exec_stmt_expr(env, expr)?,
//         Stmt::Block(stmts) => exec(Some(Env::wrap(env)), stmts)?,
//         Stmt::While(cond, stmt) => exec_stmt_while(env, cond, stmt)?,
//         // Stmt::Function(n, p, b) => exec_stmt_func(env, n, p, b)?,
//         Stmt::Return(expr) => exec_stmt_return(env, expr)?,
//     };
//     Ok(())
// }

// fn exec_stmt_return(env: EnvRef, expr: &Option<Box<Expr>>) -> Result<(), ErrorType> {
//     let res = match expr {
//         Some(expr) => eval_expr(env, expr)?,
//         None => LoxType::Nil,
//     };
//
//     Err(ErrorType::Return(res))
// }

// fn exec_stmt_func(env: EnvRef, name: &Ident, params: &[Ident], body: &[Stmt]) -> Result<(), ErrorType> {
//     let mut params = params.iter().rev();
//     let last = params.next().unwrap();
//     let mut func = Expr::Lambda(vec![last.clone()], body.to_vec());
//
//     for Ident(next) in params {
//         func = Expr::Lambda(vec![Ident(next.clone())], vec![Stmt::Return(Some(func.into()))])
//     }
//
//     env.borrow_mut()
//         .define(&name.0, &eval_expr(env.clone(), &func)?);
//
//     Ok(())
// }

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

// fn exec_stmt_expr(env: EnvRef, expr: &Expr) -> Result<(), ErrorType> {
//     eval_expr(env, expr)?;
//     Ok(())
// }

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
    }
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
    Ok(LoxType::Callable(Rc::new(LoxFn::new(param.0.clone(), body.clone().into(),
        env.clone(),
    ))))
}

fn eval_expr_call(env: EnvRef, callee: &Expr, arg: &Expr) -> Result<LoxType, ErrorType> {
    let callee = eval_expr(env.clone(), callee)?;
    let arg = eval_expr(env.clone(), arg)?;

    let res = match callee {
        LoxType::Callable(c) => c.call(&arg),
        _ => Err(ErrorType::TypeMismatch("Can't call this value")),
    };

    match res {
        Err(ErrorType::Return(res)) => Ok(res),
        other => other,
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
            Tuple(_) => todo!(),
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
