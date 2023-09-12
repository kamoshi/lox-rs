use std::fmt::Display;
use super::token_type::TokenType;


pub(crate) struct Token {
    pub(crate) ttype: TokenType,
    pub(crate) lexeme: String,
    pub(crate) line: usize,
    pub(crate) offset: usize,
    pub(crate) length: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token_type = self.ttype.to_string();
        let lexeme = &self.lexeme;
        let line = self.line;
        let start = self.offset;
        let end = self.offset + self.length;
        write!(f, "({token_type}, '{lexeme}') {line}:{start}-{end}")
    }
}
