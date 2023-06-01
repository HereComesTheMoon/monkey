use crate::tokens::{Token, TokenType};

#[derive(Clone, Copy)]
pub struct Tokenizer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut sc = Tokenizer { source, pos: 0 };
        sc.trim();
        sc
    }
}

impl Tokenizer<'_> {
    pub fn get_tokens(mut self) -> Vec<Token> {
        let mut tokens = vec![];

        while let Some(token) = self.peek() {
            self.pos += token.len;
            self.trim();
            tokens.push(token);
        }
        
        assert!(self.source.len() <= self.pos);
        self.print_tokens(&tokens);
        tokens
    }

    fn print_tokens(&self, v: &Vec<Token>) {
        println!(
            "[ {} ]",
            v.iter()
                .map(|token| " ".to_string()
                    + match token.typ {
                        TokenType::LeftParen => "(",
                        TokenType::RightParen => ")",
                        TokenType::Minus => "-",
                        TokenType::Plus => "+",
                        TokenType::Slash => "/",
                        TokenType::Star => "*",
                        TokenType::Number => {
                            &self.source[token.pos..token.pos + token.len]
                        }
                    })
                .collect::<String>()
        );
    }

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
        let s = self.source.get(pos..)?;
        let s = s.chars().next()?;
        #[rustfmt::skip]
        return Some(match s {
            '('      => Token { typ: TokenType::LeftParen,  pos, len: 1 },
            ')'      => Token { typ: TokenType::RightParen, pos, len: 1 },
            '-'      => Token { typ: TokenType::Minus,      pos, len: 1 },
            '+'      => Token { typ: TokenType::Plus,       pos, len: 1 },
            '*'      => Token { typ: TokenType::Star,       pos, len: 1 },
            '/'      => Token { typ: TokenType::Slash,      pos, len: 1 },
            '0'..='9'=> self.parse_number(),
            // ['a'..='z' | 'A'..='Z',..]                                => self.parse_identifier(pos),
            _        => {
                println!("Unexpected char: {:?} - {:#?}", s, s);
                todo!();
            }
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

        Token {
            typ: TokenType::Number,
            pos,
            len,
        }
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
            (Number    , "9".to_string()),
        ];

        for (compare_token, s) in chars.into_iter() {
            let tokenizer = Tokenizer::new(&s);
            let mut tokens = tokenizer.get_tokens();
            assert_eq!(tokens.len(), 1);
            let token = tokens.pop().unwrap();
            assert_eq!(compare_token, token.typ);
        }

    }
}
