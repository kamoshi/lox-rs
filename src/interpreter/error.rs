use crate::error::LoxError;


pub enum ErrorType {
    TypeMismatch(&'static str),
    EnvNilAccess,
}

impl LoxError for ErrorType {
    fn report(&self) {
        let message = match self {
            ErrorType::TypeMismatch(s)  => format!("Type mismatch: {s}"),
            ErrorType::EnvNilAccess     => "Variable doesn't exist".into(),
        };

        eprintln!("{message}");
    }
}
