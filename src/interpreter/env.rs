use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::error::ErrorType;
use super::types::LoxType;


type Closure = Rc<RefCell<Env>>;

pub struct Env {
    inner: HashMap<String, LoxType>,
    outer: Option<Closure>,
}

impl Env {
    pub fn new() -> Self {
        Self { inner: HashMap::new(), outer: None }
    }

    pub fn enclose(env: Closure) -> Self {
        Self { inner: HashMap::new(), outer: Some(env) }
    }

    pub fn define(&mut self, k: &str, v: &LoxType) {
        self.inner.insert(k.into(), v.clone());
    }

    pub fn get(&self, k: &str) -> Result<LoxType, ErrorType> {
        match self.inner.get(k) {
            Some(v) => Ok(v.clone()),
            None => Err(ErrorType::EnvNilAccess),
        }
    }
}
