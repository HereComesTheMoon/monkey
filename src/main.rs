use std::env;

use std::fs;
use std::io;
use std::io::{BufRead, Write};
use std::process;

fn main() {
    println!("Hello, world!");
    let args: Vec<_> = env::args().into_iter().skip(1).collect();
    println!("{:?}", args);
    match args.len() {
        0 => run_prompt(),
        1 => run_file(&args[0]),
        _ => {
            println!("Bad arguments! {:?}", args);
            process::exit(64);
        }
    }
}

fn run_file(file: &str) {
    let content = fs::read_to_string(file).unwrap();
    println!("Interpreting: {}\n", content);
    run(content);
}

fn run_prompt() {
    let stdin = io::stdin();

    print!("> ");
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        run(line);
        print!("\n> ");
        io::stdout().flush().unwrap();
    }
}

fn run(source: String) {
    let sc = Scanner::new(source);
    for token in sc.into_iter() {
        println!("{:?}", token);
        
    }
}

struct Scanner {
    source: String,
    pos: usize,
}

impl Iterator for Scanner {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.trim();
        // let next_token = self.peek();
        if let Some(token) = self.peek() {
            self.pos += token.len;
            Some(token)
        } else {
            assert!(self.source.len() <= self.pos);
            None
        }
    }
}

impl Scanner {
    fn new(source: String) -> Self {
        Scanner {
            source,
            pos: 0,
        }
    }

    fn trim(&mut self) {
        let s = self.source.get(self.pos..);
        if s.is_none() {
            assert!(self.pos <= self.source.len());
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

    fn peek(&self) -> Option<Token> {
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
struct Token {
    typ: TokenType,
    pos: usize,
    len: usize,
}

#[derive(Debug)]
enum TokenType {
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
