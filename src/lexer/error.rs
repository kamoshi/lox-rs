use crate::error::LoxError;


pub enum ErrorType {
    InvalidCharacter(char),
    UnterminatedString,
    MalformedNumber,
}

pub struct Error<'src> {
    pub ttype: ErrorType,
    pub line_str: &'src str,
    pub line: usize,
    pub offset: usize,
}

impl LoxError for Error<'_> {
    fn report(&self) {
        let line_str = self.line_str;

        use ErrorType::*;
        let message = match self.ttype {
            InvalidCharacter(char) => format!("Invalid character '{char}'"),
            UnterminatedString => "Unterminated string".into(),
            MalformedNumber => "Couldn't parse number".into(),
        };

        let marker = (0..self.offset).map(|_| ' ').collect::<String>();
        let line = self.line + 1;
        let offset = self.offset + 1;
        eprintln!("Error diagnostics:\n{line_str}\n{marker}^\nL{line}:{offset} {message}");
    }
}
