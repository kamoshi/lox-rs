use crate::error::LoxError;


pub enum ErrorType {
    TypeMismatch(&'static str),
    EnvNilAccess,
    UndefinedAssign,
}

impl LoxError for ErrorType {
    fn report(&self) {
        let message = match self {
            ErrorType::TypeMismatch(s)  => format!("Type mismatch: {s}"),
            ErrorType::EnvNilAccess     => "Variable doesn't exist".into(),
            ErrorType::UndefinedAssign  => "Tried to assign to undefined variable".into(),
        };

        eprintln!("{message}");
    }
}
