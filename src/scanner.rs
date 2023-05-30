use crate::tokenizer::{Token, Tokenizer, TokenType};

pub trait TokenStream {
    
}

#[derive(Clone)]
pub struct Lexer<'a> {
    source: &'a str,
    pub tokens: Vec<Token>, 
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let tokens = infix_to_prefix(Tokenizer::new(source));
        Lexer { source, tokens }
    }
}

pub fn infix_to_prefix(tokenizer: Tokenizer) -> Vec<Token> {
    let mut in_tokens: Vec<_> = tokenizer.into_iter().collect();
    in_tokens.reverse();

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
            continue
        }
        if token.typ == TokenType::LeftParen {
            loop {
                match stack.pop() {
                    None                                          => todo!("Unmatched parenthesis!"),
                    Some(Token { typ: TokenType::RightParen, ..}) => break,
                    Some(token)                                   => tokens.push(token),
                }
            }
            tokens.push(token);
            continue
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
                continue
            }
            break
        }
        stack.push(token);
    }

    tokens.extend(stack.into_iter().rev());

    tokens.reverse();
    println!("Final Tokens:");
    // print_tokens(&sc, &tokens);
    tokens
}

fn print_tokens(sc: &Lexer, v: &Vec<Token>) {
    println!("[ {} ]", v.iter()
        .map(|token|
            " ".to_string() + match token.typ {
                TokenType::LeftParen  => "(",
                TokenType::RightParen => ")",
                TokenType::Minus      => "-",
                TokenType::Plus       => "+",
                TokenType::Slash      => "/",
                TokenType::Star       => "*",
                TokenType::Number     => {
                    &sc.source[token.pos..token.pos+token.len]
                }
            }
        ).collect::<String>());
}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn shunting_yard1() {
        let s = "1 + 2 * 3"; // + 1 * 2 3
        let sc = Lexer::new(s.into());
        let res = sc.tokens;
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
        let s = "(1 + 2 + 3 + 4)";
        let sc = Lexer::new(s.into());
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
        let res = sc.tokens;
        assert_eq!(res, desired);
    }


    #[test]
    fn shunting_yard3() {
        // + + 3 / * 4 2 ( - 1 5 ) 6
        let s = "3 + 4 * 2 / ( 1 - 5 ) + 6";
        let sc = Lexer::new(s.into());
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

        let res = sc.tokens;
        assert_eq!(res, desired);
    }

    #[test]
    fn shunting_yard4() {
        let s = "1 - 1 - 1"; // - - 1 1 1
        let sc = Lexer::new(s.into());
        let res = sc.tokens;
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
