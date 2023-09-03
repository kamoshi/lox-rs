use crate::{token::Token, token_type::TokenType};


pub(crate) fn scan_tokens(source: &str) -> Vec<Token> {
    let chars: Vec<char> = source.chars().collect();
    let mut tokens: Vec<Token> = vec![];
    let mut current = 0;
    let mut line = 1;

    while current < chars.len() {
        let (consumed, new_line, token) = scan_token(&chars[current..]);

        if let Some(token) = token {
            tokens.push(Token {
                r#type: token,
                lexeme: String::from_iter(&chars[current..current+consumed]),
                line,
            });
        }

        if new_line { line += 1 }
        current += consumed;
    }

    tokens.push(Token {
        r#type: TokenType::Eof,
        lexeme: String::new(),
        line: 2
    });
    tokens
}

fn scan_token(
    chars: &[char]
) -> (usize, bool, Option<TokenType>) {
    let curr = chars[0];
    let next = chars.get(1);
    match curr {
        '(' => (1, false, Some(TokenType::ParenL)),
        ')' => (1, false, Some(TokenType::ParenR)),
        '{' => (1, false, Some(TokenType::BraceL)),
        '}' => (1, false, Some(TokenType::BraceR)),
        ',' => (1, false, Some(TokenType::Comma)),
        '.' => (1, false, Some(TokenType::Dot)),
        '-' => (1, false, Some(TokenType::Minus)),
        '+' => (1, false, Some(TokenType::Plus)),
        ';' => (1, false, Some(TokenType::Semicolon)),
        '*' => (1, false, Some(TokenType::Star)),
        '!' => match next {
            Some('=') => (2, false, Some(TokenType::BangEqual)),
            _ => (1, false, Some(TokenType::Bang)),
        },
        '=' => match next {
            Some('=') => (2, false, Some(TokenType::EqualEqual)),
            _ => (1, false, Some(TokenType::Equal)),
        },
        '<' => match next {
            Some('=') => (2, false, Some(TokenType::LessEqual)),
            _ => (1, false, Some(TokenType::Less)),
        },
        '>' => match next {
            Some('=') => (2, false, Some(TokenType::GreaterEqual)),
            _ => (1, false, Some(TokenType::Greater)),
        },
        '/' => match next {
            Some('/') => {
                let consumed = chars.iter()
                    .position(|&c| c == '\n')
                    .unwrap_or(chars.len());
                (consumed, false, None)
            },
            _ => (1, false, Some(TokenType::Slash)),
        }
        ' ' | '\r' | '\t' => (1, false, None),
        '\n' => (1, true, None),
        '"' => {
            let consumed = chars[1..].iter()
                .position(|&c| c == '"')
                .map(|n| n + 2)
                .expect("Unterminated string.");
            let literal = String::from_iter(&chars[1..consumed-1]);
            (consumed, false, Some(TokenType::Str(literal)))
        },
        '0'..='9' => {
            let maybe_dot = chars.iter()
                .position(|c| !c.is_numeric())
                .unwrap_or(chars.len());

            let peek = (chars.get(maybe_dot), chars.get(maybe_dot+1));
            let end = match peek {
                (Some('.'), Some('0'..='9')) => {
                    let end = chars[maybe_dot+1..].iter()
                        .position(|c| !c.is_numeric())
                        .unwrap_or(chars.len());
                    maybe_dot + 1 + end
                },
                _ => maybe_dot,
            };

            let literal: f64 = String::from_iter(&chars[..end]).parse()
                .expect("Couldn't parse number.");
            (end, false, Some(TokenType::Num(literal)))
        },
        'a'..='z' | 'A'..='Z' | '_' => {
            let consumed = chars.iter()
                .position(|&c| !c.is_alphanumeric() && c != '_')
                .unwrap_or(chars.len());
            match String::from_iter(&chars[..consumed]).as_str() {
                "and"   => TokenType::And,
                "class" => TokenType::Class,
                "else"  => TokenType::Else,
                "false" => TokenType::False,
                "for"   => TokenType::For,
                "fun"   => TokenType::Fun,
                "if"    => TokenType::If,
                "nil"   => TokenType::Nil,
                "or"    => TokenType::Or,
                "print" => TokenType::Print,
                "return"=> TokenType::Return,
                "super" => TokenType::Super,
                "this"  => TokenType::This,
                "true"  => TokenType::True,
                "var"   => TokenType::Var,
                "while" => TokenType::While,
                other   => TokenType::Ident,
            };
            (consumed, false, Some(TokenType::Ident))
        },
        _ => panic!("Unexpected character.")
    }
}
