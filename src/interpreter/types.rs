use std::{fmt::Display, rc::Rc};
use super::{env::EnvRef, error::ErrorType};


#[derive(Clone)]
pub enum LoxType {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Rc<dyn LoxCallable>),
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
        }
    }
}

pub trait LoxCallable: Display {
    fn call(&self, env: EnvRef, args: &[LoxType]) -> Result<LoxType, ErrorType>;
}

pub struct LoxFn {

}

impl LoxFn {
}

impl LoxCallable for LoxFn {
    fn call(&self, env: EnvRef, args: &[LoxType]) -> Result<LoxType, ErrorType> {
        todo!()
    }
}

impl Display for LoxFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "todo")
    }
}
