use std::fmt::Display;
use std::rc::Rc;
use super::env::EnvRef;
use super::types::{LoxCallable, LoxType};
use super::error::ErrorType;


struct FnClock;
impl LoxCallable for FnClock {
    fn call(&self, _: EnvRef, _: &[LoxType]) -> Result<LoxType, ErrorType> {
        use std::time::{SystemTime, UNIX_EPOCH};
        let epoch = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();

        Ok(LoxType::Number(epoch.as_secs_f64()))
    }
}
impl Display for FnClock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[native fun]")
    }
}

struct FnPrint;
impl LoxCallable for FnPrint {
    fn call(&self, _: EnvRef, args: &[LoxType]) -> Result<LoxType, ErrorType> {
        let args: Vec<_> = args.iter().map(|arg| arg.to_string()).collect();
        println!("{}", args.join(" "));

        Ok(LoxType::Nil)
    }
}
impl Display for FnPrint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[native fun]")
    }
}

pub fn populate(env: EnvRef) {
    env.borrow_mut().define("clock", &LoxType::Callable(Rc::new(FnClock)));
    env.borrow_mut().define("print", &LoxType::Callable(Rc::new(FnPrint)));
}
