use std::fmt::Display;
use std::rc::Rc;
use super::env::EnvRef;
use super::types::{LoxCallable, LoxType};
use super::error::ErrorType;


fn clock(_: EnvRef, _: &[LoxType]) -> Result<LoxType, ErrorType> {
    use std::time::{SystemTime, UNIX_EPOCH};
    let epoch = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();

    Ok(LoxType::Number(epoch.as_secs_f64()))
}

fn print(_: EnvRef, args: &[LoxType]) -> Result<LoxType, ErrorType> {
    let args: Vec<_> = args.iter().map(|arg| arg.to_string()).collect();
    println!("{}", args.join(" "));

    Ok(LoxType::Nil)
}


pub enum LoxFnNative {
    Clock,
    Print,
}

impl LoxCallable for LoxFnNative {
    fn call(&self, env: EnvRef, args: &[LoxType]) -> Result<LoxType, ErrorType> {
        match self {
            LoxFnNative::Clock => clock(env, args),
            LoxFnNative::Print => print(env, args),
        }
    }
}

impl Display for LoxFnNative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[native fn]")
    }
}

pub fn populate(env: EnvRef) {
    env.borrow_mut().define("clock", &LoxType::Callable(Rc::new(LoxFnNative::Clock)));
    env.borrow_mut().define("print", &LoxType::Callable(Rc::new(LoxFnNative::Print)));
}
