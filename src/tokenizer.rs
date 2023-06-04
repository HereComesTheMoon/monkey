use crate::tokens::{Token, TokenType};

#[derive(Clone)]
pub struct Tokenizer {
    source: String,
    pos: usize,
}

impl Tokenizer {
    pub fn new(source: String) -> Self {
        let mut sc = Tokenizer { source, pos: 0 };
        sc.trim();
        sc
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(token) = self.peek() {
            self.pos += token.len;
            self.trim();
            return Some(token);
        }
        
        assert!(self.source.len() <= self.pos);
        None
    }
}

impl Tokenizer {
    fn trim(&mut self) {
        let s = self.source.get(self.pos..);
        if s.is_none() {
            assert!(self.pos < self.source.len());
            return;
        }
        self.pos += s
            .unwrap()
            .char_indices()
            .take_while(|(_, c)| c.is_whitespace())
            .map(|(i, _)| i + 1)
            .last()
            .unwrap_or(0);
    }

    fn peek(&self) -> Option<Token> {
        let pos = self.pos;
        let s = &self.source[pos..];

        if s.is_empty() {
            return None;
        }

        let mut prefix = [' '; 7];
        for (k, c) in s.chars().take(7).enumerate() {
            prefix[k] = c;
        }

        #[rustfmt::skip]
        return Some(match prefix {
            ['(',..]                                                  => Token { typ: TokenType::LeftParen,    pos, len: 1 },
            [')',..]                                                  => Token { typ: TokenType::RightParen,   pos, len: 1 },
            ['{',..]                                                  => Token { typ: TokenType::LeftBrace,    pos, len: 1 },
            ['}',..]                                                  => Token { typ: TokenType::RightBrace,   pos, len: 1 },
            [',',..]                                                  => Token { typ: TokenType::Comma,        pos, len: 1 },
            ['.',..]                                                  => Token { typ: TokenType::Dot,          pos, len: 1 },
            ['-',..]                                                  => Token { typ: TokenType::Minus,        pos, len: 1 },
            ['+',..]                                                  => Token { typ: TokenType::Plus,         pos, len: 1 },
            [';',..]                                                  => Token { typ: TokenType::Semicolon,    pos, len: 1 },
            ['*',..]                                                  => Token { typ: TokenType::Star,         pos, len: 1 },
            ['>','=',..]                                              => Token { typ: TokenType::GreaterEqual, pos, len: 2 },
            ['>',..]                                                  => Token { typ: TokenType::Greater,      pos, len: 1 },
            ['<','=',..]                                              => Token { typ: TokenType::LessEqual,    pos, len: 2 },
            ['<',..]                                                  => Token { typ: TokenType::Less,         pos, len: 1 },
            ['!','=',..]                                              => Token { typ: TokenType::BangEqual,    pos, len: 2 },
            ['!',..]                                                  => Token { typ: TokenType::Bang,         pos, len: 1 },
            ['=','=',..]                                              => Token { typ: TokenType::EqualEqual,   pos, len: 2 },
            ['=',..]                                                  => Token { typ: TokenType::Equal,        pos, len: 1 },
            ['/',..]                                                  => Token { typ: TokenType::Slash,        pos, len: 1 },
            ['a','n','d',..]             if prefix[3].is_whitespace() => Token { typ: TokenType::And,          pos, len: 3 },
            ['e','l','s','e',..]         if prefix[4].is_whitespace() => Token { typ: TokenType::Else,         pos, len: 4 },
            ['f','a','l','s','e',..]     if prefix[5].is_whitespace() => Token { typ: TokenType::False,        pos, len: 5 },
            ['f','n',..]                 if prefix[2].is_whitespace() => Token { typ: TokenType::Fun,          pos, len: 2 },
            ['i','f',..]                 if prefix[2].is_whitespace() => Token { typ: TokenType::If,           pos, len: 2 },
            ['l','e','t',..]             if prefix[3].is_whitespace() => Token { typ: TokenType::Let,          pos, len: 3 },
            ['o','r',..]                 if prefix[2].is_whitespace() => Token { typ: TokenType::Or,           pos, len: 2 },
            ['r','e','t','u','r','n',..] if prefix[6].is_whitespace() => Token { typ: TokenType::Return,       pos, len: 6 },
            ['t','r','u','e',..]         if prefix[4].is_whitespace() => Token { typ: TokenType::True,         pos, len: 4 },
            ['0'..='9',..]                                            => self.parse_number(),
            ['"',..]                                                  => self.parse_string(),
            ['a'..='z' | 'A'..='Z',..]                                => self.parse_identifier(),
            t                                                         => todo!("{t:?}"),
        });
    }

    fn parse_number(&self) -> Token {
        let pos = self.pos;
        let s = self.source.get(pos..).unwrap();
        assert!(s.chars().next().unwrap().is_digit(10));

        let len = s
            .char_indices()
            .take_while(|(_, c)| c.is_digit(10))
            .last()
            .map(|(i, _)| i + 1)
            .unwrap();

        let typ = if let Ok(number) = s[..len].parse::<i64>() {
            TokenType::Number(number)
        } else {
            TokenType::Error
        };

        Token {
            typ,
            pos,
            len,
        }
    }

    fn parse_identifier(&self) -> Token {
        let pos = self.pos;
        let s = self.source.get(pos..).unwrap();
        let len = 1 + s
        .char_indices()
        .take_while(|(_,c)| c.is_ascii_alphanumeric())
        .last()
        .map(|(i,_)| i)
        .unwrap();
        Token {
            typ: TokenType::Identifier(s[..len].to_string()),
            pos,
            len,
        }
    }

    fn parse_string(&self) -> Token {
        let pos = self.pos;
        let s = self.source.get(pos..).unwrap();
        assert_eq!(s.chars().next().unwrap(), '"');

        let mut escape = false;
        for (i, c) in s.char_indices().skip(1) {
            if escape {
                escape = false;
                continue;
            }
            if c == '\\' {
                escape = true;
                continue;
            }
            if c == '"' {
                let len = i + 1;
                return Token {
                    typ: TokenType::String(s[..len].to_string()),
                    pos,
                    len,
                };
            }
        }
        return Token {
            typ: TokenType::Error,
            pos,
            len: pos + s.chars().count(),
        };
    }
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokens() {
        use TokenType::*;
        let chars = vec![
            (LeftParen , "(".to_string()),
            (RightParen, ")".to_string()),
            (Minus     , "-".to_string()),
            (Plus      , "+".to_string()),
            (Slash     , "/".to_string()),
            (Star      , "*".to_string()),
            // (Number    , "9".to_string()),
        ];

        for (compare_token, s) in chars.into_iter() {
            let tokenizer = Tokenizer::new(s);
            let mut tokens: Vec<_> = tokenizer.collect();
            assert_eq!(tokens.len(), 1);
            let token = tokens.pop().unwrap();
            assert_eq!(compare_token, token.typ);
        }

    }
}
