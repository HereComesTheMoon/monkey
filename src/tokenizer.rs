use std::cmp::Ordering;

#[derive(Clone)]
pub struct Tokenizer<'a> {
    source: &'a str,
    pos: usize,
}


impl Iterator for Tokenizer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.peek() {
            self.pos += token.len;
            self.trim();
            Some(token)
        } else {
            assert!(self.source.len() <= self.pos);
            None
        }
    }
}

// fn print_tokens(sc: &Lexer, v: &Vec<Token>) {
//     println!("[ {} ]", v.iter()
//         .map(|token|
//             " ".to_string() + match token.typ {
//                 TokenType::LeftParen  => "(",
//                 TokenType::RightParen => ")",
//                 TokenType::Minus      => "-",
//                 TokenType::Plus       => "+",
//                 TokenType::Slash      => "/",
//                 TokenType::Star       => "*",
//                 TokenType::Number     => {
//                     &sc.source[token.pos..token.pos+token.len]
//                 }
//             }
//         ).collect::<String>());
// }

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut sc = Tokenizer { source, pos: 0 };
        sc.trim();
        sc
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

    pub fn peek(&self) -> Option<Token> {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub pos: usize,
    pub len: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    Minus,
    Plus,
    Slash,
    Star,

    // Number
    Number,
    // EOF, // unnecessary token?
}

impl TokenType {
    fn precedence(&self) -> u8 {
        #[rustfmt::skip]
        return match &self {
            TokenType::LeftParen  => 4,
            TokenType::RightParen => 0,
            TokenType::Minus      => 2,
            TokenType::Plus       => 2,
            TokenType::Slash      => 3,
            TokenType::Star       => 3,
            TokenType::Number     => 9,
        };
    }
}

impl PartialOrd for TokenType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.precedence().cmp(&other.precedence()))
    }
}

impl Ord for TokenType {
    fn cmp(&self, other: &Self) -> Ordering {
        TokenType::partial_cmp(&self, other).unwrap()
    }
}

