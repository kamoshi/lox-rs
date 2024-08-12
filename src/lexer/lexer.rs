use crate::lexer::{token::Token, token_type::TokenType};
use super::error::{Error, ErrorType};


pub fn tokenize(source: &str) -> Result<Vec<Token>, Error> {
    let mut tokens = vec![];

    for (line, line_str) in source.lines().enumerate() {
        let chars = line_str.chars().collect::<Vec<_>>();
        let mut offset = 0;

        while offset < chars.len() {
            let (length, token) = match read_token(&chars[offset..]) {
                Ok(lexed) => lexed,
                Err(ttype) => return Err(Error { ttype, line, offset }),
            };

            if let Some(ttype) = token {
                tokens.push(Token { ttype, line, offset, length });
            }

            offset += length;
        }
    }

    tokens.push(Token {
        ttype: TokenType::Eof,
        line: tokens.last().map(|t| t.line + 1).unwrap_or(1),
        offset: 0,
        length: 0
    });

    Ok(tokens)
}

fn read_token(
    chars: &[char]
) -> Result<(usize, Option<TokenType>), ErrorType> {
    let curr = chars[0];
    let next = chars.get(1);
    match curr {
        // single char
        '(' | ')' | '{' | '}' | '[' | ']' | ',' | '.' | ';' => {
            let ttype = match curr {
                '(' => TokenType::ParenL,
                ')' => TokenType::ParenR,
                '{' => TokenType::BraceL,
                '}' => TokenType::BraceR,
                '[' => TokenType::SquareL,
                ']' => TokenType::SquareR,
                ',' => TokenType::Comma,
                '.' => TokenType::Dot,
                ';' => TokenType::Semicolon,
                _ => unreachable!(),
            };
            Ok((1, Some(ttype)))
        }
        // div | comment
        '/' => match next {
            Some('/') => {
                let consumed = chars.iter().position(|&c| c == '\n').unwrap_or(chars.len());
                Ok((consumed, None))
            },
            _ => Ok((1, Some(TokenType::Op("/".into())))),
        }
        // whitespace
        ' ' | '\r' | '\t' => Ok((1, None)),
        // strings
        '"' => {
            let consumed = chars[1..].iter().position(|&c| c == '"').map(|n| n + 2);
            let consumed = match consumed {
                Some(n) => n,
                None => return Err(ErrorType::UnterminatedString),
            };
            let literal = String::from_iter(&chars[1..consumed - 1]);
            Ok((consumed, Some(TokenType::Str(literal))))
        },
        // numbers
        '0'..='9' => {
            fn find_non_digit(chars: &[char]) -> usize {
                chars.iter()
                    .position(|c| !c.is_ascii_digit())
                    .unwrap_or(chars.len())
            }

            let fst_end = find_non_digit(chars);

            let end = match (chars.get(fst_end), chars.get(fst_end + 1)) {
                (Some('.'), Some('0'..='9')) => {
                    let snd_end = find_non_digit(&chars[fst_end + 1..]);
                    fst_end + 1 + snd_end
                },
                _ => fst_end,
            };

            let literal = String::from_iter(&chars[..end]).parse();
            let literal = match literal {
                Ok(n) => n,
                Err(_) => return Err(ErrorType::MalformedNumber),
            };
            Ok((end, Some(TokenType::Num(literal))))
        },
        // identifiers
        'a'..='z' | 'A'..='Z' | '_' => {
            let consumed = chars.iter()
                .position(|&c| !c.is_alphanumeric() && c != '_')
                .unwrap_or(chars.len());
            let token_type = match String::from_iter(&chars[..consumed]).as_str() {
                "and"   => TokenType::And,
                "class" => TokenType::Class,
                "else"  => TokenType::Else,
                "false" => TokenType::False,
                "for"   => TokenType::For,
                "fn"    => TokenType::Fun,
                "if"    => TokenType::If,
                "nil"   => TokenType::Nil,
                "or"    => TokenType::Or,
                "return"=> TokenType::Return,
                "super" => TokenType::Super,
                "this"  => TokenType::This,
                "true"  => TokenType::True,
                "var"   => TokenType::Var,
                "while" => TokenType::While,
                "variant"   => TokenType::Variant,
                "typeclass" => TokenType::Typeclass,
                "instance"  => TokenType::Instance,
                other   => TokenType::Ident(String::from(other)),
            };
            Ok((consumed, Some(token_type)))
        },
        _ => {
            let consumed = chars.iter()
                .position(|&c| !c.is_ascii_punctuation() || c == '_')
                .unwrap_or(chars.len());

            let text = String::from_iter(&chars[..consumed]);

            Ok((consumed, Some(TokenType::Op(text))))
        },
    }
}
