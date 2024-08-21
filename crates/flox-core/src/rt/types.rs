use crate::parse::ast::Expr;
use core::panic;
use std::{collections::HashMap, fmt::Display, rc::Rc};

use super::{
    env::{Env, EnvRef},
    error::ErrorType,
    eval_expr,
};

#[derive(Clone)]
pub enum LoxType {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<LoxType>),
    Tuple(Box<[LoxType]>),
    Callable(Callable),
    Data(usize, usize, Box<[LoxType]>),
    Type(usize, HashMap<String, LoxType>),
}

#[derive(Clone)]
pub(crate) enum Callable {
    Function(Rc<dyn LoxCallable>),
    Constructor(usize, usize, Option<Rc<dyn LoxCallable>>),
}

impl Callable {
    pub(crate) fn new(func: Rc<dyn LoxCallable>) -> Self {
        Self::Function(func)
    }

    #[inline(always)]
    pub(crate) fn call(&self, arg: &LoxType) -> Result<LoxType, ErrorType> {
        let res = match self {
            Callable::Function(func) => func.call(arg),
            Callable::Constructor(_, _, f) => match f {
                Some(f) => f.call(arg),
                None => panic!("This data constructor is not callable"),
            },
        };
        match res {
            Err(ErrorType::Return(res)) => Ok(res),
            other => other,
        }
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Callable::Function(_) => write!(f, "[fn]"),
            Callable::Constructor(_, _, _) => write!(f, "[constructor fn]"),
        }
    }
}

impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LoxType::*;
        match self {
            Nil => write!(f, "nil"),
            Boolean(b) => write!(f, "{b}"),
            Number(n) => write!(f, "{n}"),
            String(s) => write!(f, "{s}"),
            Callable(c) => write!(f, "{c}"),
            Array(a) => {
                write!(f, "[")?;
                let mut iter = a.iter();
                if let Some(first) = iter.next() {
                    write!(f, "{}", first)?;
                    for elem in iter {
                        write!(f, ", {}", elem)?;
                    }
                }
                write!(f, "]")
            }
            Tuple(values) => {
                write!(f, "(")?;
                let mut iter = values.iter();
                if let Some(first) = iter.next() {
                    write!(f, "{}", first)?;
                    for elem in iter {
                        write!(f, ", {}", elem)?;
                    }
                }
                write!(f, ")")
            },
            Type(ty, _) => write!(f, "type {ty}"),
            Data(a, b, vals) => write!(f, "data {a} {b} {}", vals.len()),
        }
    }
}

impl PartialEq for LoxType {
    fn eq(&self, other: &Self) -> bool {
        use core::mem::discriminant;
        match (self, other) {
            (Self::Boolean(l), Self::Boolean(r)) => l == r,
            (Self::Number(l), Self::Number(r)) => l == r,
            (Self::String(l), Self::String(r)) => l == r,
            (Self::Callable(l), Self::Callable(r)) => false, // Rc::ptr_eq(l, r)
            _ => discriminant(self) == discriminant(other),
        }
    }
}

impl LoxType {
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxType::Nil => false,
            LoxType::Boolean(b) => *b,
            LoxType::Number(_) => true,
            LoxType::String(_) => true,
            LoxType::Callable(_) => true,
            LoxType::Array(_) => false,
            LoxType::Tuple(_) => false,
            _ => false,
        }
    }

    pub(crate) fn get_type(&self) -> (usize, usize) {
        match self {
            LoxType::Type(_, _)    => (0, 0),
            LoxType::Nil           => (1, 0),
            LoxType::Boolean(_)    => (2, 0),
            LoxType::Number(_)     => (3, 0),
            LoxType::String(_)     => (4, 0),
            LoxType::Array(_)      => (5, 0),
            LoxType::Tuple(_)      => (6, 0),
            LoxType::Callable(c)   => match c {
                Callable::Function(_) => (7, 0),
                Callable::Constructor(t, v, _) => (*t, *v),
            },
            LoxType::Data(t, v, _) => (*t, *v),
        }
    }
}

pub trait LoxCallable {
    fn call(&self, args: &LoxType) -> Result<LoxType, ErrorType>;
}

pub struct LoxFn {
    param: String,
    body: Box<Expr>,
    closure: EnvRef,
}

impl LoxFn {
    pub fn new(param: String, body: Box<Expr>, closure: EnvRef) -> Self {
        Self {
            param,
            body,
            closure,
        }
    }
}

impl LoxCallable for LoxFn {
    fn call(&self, arg: &LoxType) -> Result<LoxType, ErrorType> {
        let env = Env::wrap(self.closure.clone());
        env.borrow_mut().define(&self.param, arg);
        eval_expr(env, &self.body)
    }
}

impl Display for LoxFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[fn]")
    }
}
