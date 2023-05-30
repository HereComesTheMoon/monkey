use std::cmp::Ordering;

#[derive(Clone)]
pub struct Tokenizer<'a> {
    source: &'a str,
    pos: usize,
}

impl Tokenizer<'_> {
    pub fn get_infix(self) -> Vec<Token> {
        self.into_iter().collect()
    }

    pub fn infix_to_prefix(self) -> Vec<Token> {
        let mut in_tokens = self.get_infix();
        in_tokens.reverse();
        let in_tokens = in_tokens;

        let mut tokens = vec![];

        let mut stack: Vec<Token> = vec![];

        for token in in_tokens.into_iter() {
            // println!("{}", stack);
            // println!("Tokens:");
            // println!("{:?}", token.typ);
            // print_tokens(&sc, &tokens);
            // print_tokens(&sc, &stack);
            if token.typ == TokenType::RightParen {
                tokens.push(token.clone());
                stack.push(token);
                continue;
            }
            if token.typ == TokenType::LeftParen {
                loop {
                    match stack.pop() {
                        None => todo!("Unmatched parenthesis!"),
                        Some(Token {
                            typ: TokenType::RightParen,
                            ..
                        }) => break,
                        Some(token) => tokens.push(token),
                    }
                }
                tokens.push(token);
                continue;
            }

            while !stack.is_empty() {
                // TODO: Think about operator associativity
                // Right now everything is left associative, which is weird for exponentials
                // Consider splitting precedence into left_prec and right_prec
                // let top = stack.last().unwrap().typ;
                // match TokenType::cmp(&token.typ, top) {
                //     Ordering::Less    => tokens.push(stack.pop().unwrap()),
                //     Ordering::Equal   => todo!(),
                //     Ordering::Greater => todo!(),
                // };
                if token.typ < stack.last().unwrap().typ {
                    tokens.push(stack.pop().unwrap());
                    continue;
                }
                break;
            }
            stack.push(token);
        }

        tokens.extend(stack.into_iter().rev());

        tokens.reverse();
        println!("Final Tokens:");
        // print_tokens(&sc, &tokens);
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn shunting_yard1() {
        let source = "1 + 2 * 3".into(); // + 1 * 2 3
        let tokenizer = Tokenizer::new(source);
        let res = tokenizer.infix_to_prefix();
        let desired = vec![
            Token {
                typ: TokenType::Plus,
                pos: 2,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 0,
                len: 1,
            },
            Token {
                typ: TokenType::Star,
                pos: 6,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 4,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 8,
                len: 1,
            },
        ];

        assert_eq!(res, desired);
    }

    #[test]
    fn shunting_yard2() {
        let source = "(1 + 2 + 3 + 4)".into();
        let tokenizer = Tokenizer::new(source);
        let res = tokenizer.infix_to_prefix();
        let desired = vec![
            Token {
                typ: TokenType::LeftParen,
                pos: 0,
                len: 1,
            },
            Token {
                typ: TokenType::Plus,
                pos: 11,
                len: 1,
            },
            Token {
                typ: TokenType::Plus,
                pos: 7,
                len: 1,
            },
            Token {
                typ: TokenType::Plus,
                pos: 3,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 1,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 5,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 9,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 13,
                len: 1,
            },
            Token {
                typ: TokenType::RightParen,
                pos: 14,
                len: 1,
            },
        ];
        // let res = sc.tokens;
        assert_eq!(res, desired);
    }

    #[test]
    fn shunting_yard3() {
        // + + 3 / * 4 2 ( - 1 5 ) 6
        let source = "3 + 4 * 2 / ( 1 - 5 ) + 6".into();
        let tokenizer = Tokenizer::new(source);
        let res = tokenizer.infix_to_prefix();
        let desired = vec![
            Token {
                typ: TokenType::Plus,
                pos: 22,
                len: 1,
            },
            Token {
                typ: TokenType::Plus,
                pos: 2,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 0,
                len: 1,
            },
            Token {
                typ: TokenType::Slash,
                pos: 10,
                len: 1,
            },
            Token {
                typ: TokenType::Star,
                pos: 6,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 4,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 8,
                len: 1,
            },
            Token {
                typ: TokenType::LeftParen,
                pos: 12,
                len: 1,
            },
            Token {
                typ: TokenType::Minus,
                pos: 16,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 14,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 18,
                len: 1,
            },
            Token {
                typ: TokenType::RightParen,
                pos: 20,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 24,
                len: 1,
            },
        ];

        assert_eq!(res, desired);
    }

    #[test]
    fn shunting_yard4() {
        let source = "1 - 1 - 1".into(); // - - 1 1 1
        let tokenizer = Tokenizer::new(source);
        let res = tokenizer.infix_to_prefix();
        let desired = vec![
            Token {
                typ: TokenType::Minus,
                pos: 6,
                len: 1,
            },
            Token {
                typ: TokenType::Minus,
                pos: 2,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 0,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 4,
                len: 1,
            },
            Token {
                typ: TokenType::Number,
                pos: 8,
                len: 1,
            },
        ];

        assert_eq!(res, desired);
    }
}
