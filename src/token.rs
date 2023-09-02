use crate::token_type::TokenType;


pub struct Token {
    r#type: TokenType,
    lexeme: String,
    literal: String,
    line: i32,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        format!("{} {} {}", self.r#type, self.lexeme, self.literal)
    }
}
