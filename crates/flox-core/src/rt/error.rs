use crate::error::FloxError;
use super::types::LoxType;


pub enum RuntimeErrorKind {
    TypeMismatch(&'static str),
    EnvNilAccess,
    UndefinedAssign,
    Return(LoxType)
}

impl FloxError for RuntimeErrorKind {
    fn report(&self) -> String {
        let message = match self {
            RuntimeErrorKind::TypeMismatch(s)  => format!("Type mismatch: {s}"),
            RuntimeErrorKind::EnvNilAccess     => "Variable doesn't exist".into(),
            RuntimeErrorKind::UndefinedAssign  => "Tried to assign to undefined variable".into(),
            RuntimeErrorKind::Return(_)        => "Return can only be called in functions".into(),
        };

        format!("{message}")
    }

    fn report_rich(&self, _: &str) -> String {
        self.report()
    }
}
