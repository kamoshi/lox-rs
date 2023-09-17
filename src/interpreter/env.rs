use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::error::ErrorType;
use super::types::LoxType;


pub type EnvRef = Rc<RefCell<Env>>;

pub struct Env {
    inner: HashMap<String, LoxType>,
    outer: Option<EnvRef>,
}

impl Env {
    pub fn new() -> Env {
        Self { inner: HashMap::new(), outer: None }
    }

    pub fn new_ref() -> EnvRef {
        Rc::new(RefCell::new(Self::new()))
    }

    pub fn wrap(env: EnvRef) -> EnvRef {
        Rc::new(RefCell::new(Self { inner: HashMap::new(), outer: Some(env) }))
    }

    pub fn define(&mut self, k: &str, v: &LoxType) {
        self.inner.insert(k.into(), v.clone());
    }

    pub fn get(&self, k: &str) -> Result<LoxType, ErrorType> {
        match (self.inner.get(k), &self.outer) {
            (Some(v), _)        => Ok(v.clone()),
            (None, Some(outer)) => outer.borrow().get(k),
            (None, None)        => Err(ErrorType::EnvNilAccess),
        }
    }

    pub fn set(&mut self, k: &str, v: &LoxType) -> Result<LoxType, ErrorType> {
        match (self.inner.contains_key(k), &self.outer) {
            (true, _)               => {
                self.inner.insert(k.into(), v.clone());
                Ok(v.clone())
            },
            (false, Some(outer))    => outer.borrow_mut().set(k, v),
            (false, None)           => Err(ErrorType::UndefinedAssign),
        }
    }
}
