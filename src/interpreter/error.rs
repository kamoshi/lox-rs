use crate::error::LoxError;
use super::types::LoxType;


pub enum ErrorType {
    TypeMismatch(&'static str),
    EnvNilAccess,
    UndefinedAssign,
    Return(LoxType)
}

impl LoxError for ErrorType {
    fn report(&self) {
        let message = match self {
            ErrorType::TypeMismatch(s)  => format!("Type mismatch: {s}"),
            ErrorType::EnvNilAccess     => "Variable doesn't exist".into(),
            ErrorType::UndefinedAssign  => "Tried to assign to undefined variable".into(),
            ErrorType::Return(_)        => "Return can only be called in functions".into(),
        };

        eprintln!("{message}");
    }

    fn report_rich(&self, source: &str) {
        self.report()
    }
}
