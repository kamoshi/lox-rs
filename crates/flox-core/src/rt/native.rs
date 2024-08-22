use super::env::EnvRef;
use super::error::RuntimeErrorKind;
use super::types::{Callable, LoxCallable, LoxType};
use std::fmt::Display;
use std::rc::Rc;

fn clock(_: &LoxType) -> Result<LoxType, RuntimeErrorKind> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let epoch = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
    Ok(LoxType::Number(epoch.as_secs_f64()))
}

fn print(arg: &LoxType) -> Result<LoxType, RuntimeErrorKind> {
    println!("{arg}");
    Ok(LoxType::Nil)
}

pub enum LoxFnNative {
    Clock,
    Print,
}

impl LoxCallable for LoxFnNative {
    fn call(&self, arg: &LoxType) -> Result<LoxType, RuntimeErrorKind> {
        match self {
            LoxFnNative::Clock => clock(arg),
            LoxFnNative::Print => print(arg),
        }
    }
}

impl Display for LoxFnNative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[native fn]")
    }
}

pub fn populate(env: EnvRef) {
    let mut env = env.borrow_mut();
    env.define("clock", &LoxType::Callable(Callable::new(Rc::new(LoxFnNative::Clock))));
    env.define("print", &LoxType::Callable(Callable::new(Rc::new(LoxFnNative::Print))));
}
