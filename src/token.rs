use crate::token_type::TokenType;


pub(crate) struct Token {
    pub(crate) r#type: TokenType,
    pub(crate) lexeme: String,
    pub(crate) line: i32,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        format!("{} {}", self.r#type, self.lexeme)
    }
}
