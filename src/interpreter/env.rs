use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::error::ErrorType;
use super::types::LoxType;

struct Registry {
    types: HashMap<usize, String>,
}

pub type EnvRef = Rc<RefCell<Env>>;

pub struct Env {
    inner: HashMap<String, LoxType>,
    outer: Option<EnvRef>,
    registry: Rc<RefCell<Registry>>,

}

impl Env {
    pub fn new() -> Env {
        Self { inner: HashMap::new(), outer: None, registry: RefCell::new(Registry { types: HashMap::new() }).into() }
    }

    pub fn new_ref() -> EnvRef {
        Rc::new(RefCell::new(Self::new()))
    }

    pub fn wrap(env: EnvRef) -> EnvRef {
        let registry = env.borrow().registry.clone();
        Rc::new(RefCell::new(Self { inner: HashMap::new(), registry, outer: Some(env) }))
    }

    pub fn define(&mut self, k: &str, v: &LoxType) {
        self.inner.insert(k.into(), v.clone());
    }

    pub fn get(&self, k: &str) -> Result<LoxType, ErrorType> {
        match (self.inner.get(k), &self.outer) {
            (Some(v), _)        => Ok(v.clone()),
            (None, Some(outer)) => outer.borrow().get(k),
            (None, None)        => { println!("{k}"); Err(ErrorType::EnvNilAccess) },
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

    pub fn new_type(&self, name: String) -> usize {
        let mut reg = self.registry.borrow_mut();
        let len = reg.types.len();
        reg.types.insert(len, name);
        len
    }

    pub fn get_type(&self, id: usize) -> Option<String> {
        self.registry.borrow().types.get(&id).cloned()
    }
}
