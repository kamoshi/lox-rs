use std::borrow::Cow;

use crate::error::FloxError;


pub enum LexerErrorKind {
    UnterminatedString,
    MalformedNumber,
}

pub struct LexerError {
    pub kind: LexerErrorKind,
    pub line: usize,
    pub offset: usize,
}

impl FloxError for LexerError {
    fn report(&self) -> String {
        let line = self.line + 1;
        let offset = self.offset + 1;

        use LexerErrorKind::*;
        let message: Cow<str> = match self.kind {
            UnterminatedString      => "Unterminated string".into(),
            MalformedNumber         => "Couldn't parse number".into(),
        };

        format!("Lexer error:\nL{line}:{offset} {message}")
    }

    fn report_rich(&self, source: &str) -> String {
        let report = self.report();

        let line_str = source.lines().nth(self.line).unwrap();
        let marker = (0..self.offset).map(|_| ' ').collect::<String>();

        format!("{report}\n\n{line_str}\n{marker}^")
    }
}
