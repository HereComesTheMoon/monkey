#[derive(Clone)]
pub struct Scanner {
    source: String,
    pos: usize,
}

impl Iterator for Scanner {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // let next_token = self.peek();
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

impl Scanner {
    pub fn new(source: String) -> Self {
        let mut sc = Scanner {
            source,
            pos: 0,
        };
        sc.trim();
        sc
    }

    fn trim(&mut self) {
        let s = self.source.get(self.pos..);
        if s.is_none() {
            assert!(self.pos < self.source.len());
            return
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
            '('      => Token { typ: TokenType::LeftParen,    pos, len: 1 },
            ')'      => Token { typ: TokenType::RightParen,   pos, len: 1 },
            '-'      => Token { typ: TokenType::Minus,        pos, len: 1 },
            '+'      => Token { typ: TokenType::Plus,         pos, len: 1 },
            '*'      => Token { typ: TokenType::Star,         pos, len: 1 },
            '/'      => Token { typ: TokenType::Slash,        pos, len: 1 },
            '0'..='9'=> self.parse_number(),
            // ['a'..='z' | 'A'..='Z',..]                                => self.parse_identifier(pos),
            _                                                         => {
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
            .map(|(i,_)| i + 1)
            .unwrap();

        Token {
            typ: TokenType::Number,
            pos,
            len,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub typ: TokenType,
    pub pos: usize,
    pub len: usize,
}

#[derive(Debug)]
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

