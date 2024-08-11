use std::{fmt::Display, rc::Rc};
use crate::parser::ast::Stmt;

use super::{env::{EnvRef, Env}, error::ErrorType, exec};


#[derive(Clone)]
pub enum LoxType {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Rc<dyn LoxCallable>),
    Array(Vec<LoxType>),
}


impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LoxType::*;
        match self {
            Nil         => write!(f, "nil"),
            Boolean(b)  => write!(f, "{b}"),
            Number(n)   => write!(f, "{n}"),
            String(s)   => write!(f, "{s}"),
            Callable(c) => write!(f, "{c}"),
            Array(a)    => {
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
        }
    }
}

impl PartialEq for LoxType {
    fn eq(&self, other: &Self) -> bool {
        use core::mem::discriminant;
        match (self, other) {
            (Self::Boolean(l), Self::Boolean(r))    => l == r,
            (Self::Number(l), Self::Number(r))      => l == r,
            (Self::String(l), Self::String(r))      => l == r,
            (Self::Callable(l), Self::Callable(r))  => Rc::ptr_eq(l, r),
            _ => discriminant(self) == discriminant(other),
        }
    }
}

impl LoxType {
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxType::Nil            => false,
            LoxType::Boolean(b)     => *b,
            LoxType::Number(_)      => true,
            LoxType::String(_)      => true,
            LoxType::Callable(_)    => true,
            LoxType::Array(_)       => false,
        }
    }
}

pub trait LoxCallable: Display {
    fn call(&self, args: &[LoxType]) -> Result<LoxType, ErrorType>;
}

pub struct LoxFn {
    params: Vec<String>,
    body: Vec<Stmt>,
    closure: EnvRef,
}

impl LoxFn {
    pub fn new(params: Vec<String>, body: Vec<Stmt>, closure: EnvRef) -> Self {
        Self { params, body, closure }
    }
}

impl LoxCallable for LoxFn {
    fn call(&self, args: &[LoxType]) -> Result<LoxType, ErrorType> {
        let env = Env::wrap(self.closure.clone());

        for (k, v) in self.params.iter().zip(args) {
            env.borrow_mut().define(k, v);
        }

        for k in self.params.iter().skip(args.len()) {
            env.borrow_mut().define(k, &LoxType::Nil);
        }

        exec(Some(env), &self.body)?;

        Ok(LoxType::Nil)
    }
}

impl Display for LoxFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[fn]")
    }
}
