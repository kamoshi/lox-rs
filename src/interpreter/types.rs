use std::fmt::Display;


#[derive(Clone, PartialEq)]
pub enum LoxType {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Display for LoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LoxType::*;
        match self {
            Nil         => write!(f, "nil"),
            Boolean(b)  => write!(f, "{b}"),
            Number(n)   => write!(f, "{n}"),
            String(s)   => write!(f, "{s}"),
        }
    }
}

impl LoxType {
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxType::Nil        => false,
            LoxType::Boolean(b) => *b,
            LoxType::Number(_)  => false,
            LoxType::String(_)  => false,
        }
    }

    //pub fn is_falsy(&self) -> bool {
    //    !self.is_truthy()
    //}
}
