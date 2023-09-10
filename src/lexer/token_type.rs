#[derive(Debug)]
pub(crate) enum TokenType {

    // Single-character tokens
    ParenL, ParenR, BraceL, BraceR,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals
    Ident(String), Str(String), Num(f64),

    // Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof,
}

impl ToString for TokenType {
    fn to_string(&self) -> String {
        use TokenType::*;
        match self {
            ParenL          => String::from("paren-l"),
            ParenR          => String::from("paren-r"),
            BraceL          => String::from("brace-l"),
            BraceR          => String::from("brace-r"),
            Comma           => String::from("comma"),
            Dot             => String::from("dot"),
            Minus           => String::from("minus"),
            Plus            => String::from("plus"),
            Semicolon       => String::from("semicolon"),
            Slash           => String::from("slash"),
            Star            => String::from("star"),
            Bang            => String::from("bang"),
            BangEqual       => String::from("bang-equal"),
            Equal           => String::from("equal"),
            EqualEqual      => String::from("equal-equal"),
            Greater         => String::from("greater"),
            GreaterEqual    => String::from("greater-equal"),
            Less            => String::from("less"),
            LessEqual       => String::from("less-equal"),
            Ident(ident)    => format!("identifier: {ident}"),
            Str(str)        => format!("string: {str}"),
            Num(num)        => format!("number: {num}"),
            And             => String::from("and"),
            Class           => String::from("class"),
            Else            => String::from("else"),
            False           => String::from("false"),
            Fun             => String::from("fun"),
            For             => String::from("for"),
            If              => String::from("if"),
            Nil             => String::from("nil"),
            Or              => String::from("or"),
            Print           => String::from("print"),
            Return          => String::from("return"),
            Super           => String::from("super"),
            This            => String::from("this"),
            True            => String::from("true"),
            Var             => String::from("var"),
            While           => String::from("while"),
            Eof             => String::from("EOF"),
        }
    }
}
