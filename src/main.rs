mod tokenizer;
mod tokens;
mod parser;
use std::assert_eq;
use std::env;

use std::fs;
use std::io;
use std::io::{BufRead, Write};
use std::println;
use std::process;
use tokenizer::Tokenizer;
use parser::{Parser, Error, ErrorLocation};

fn main() {
    println!("Hello, world!");
    let args: Vec<_> = env::args().skip(1).collect();
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
    println!("Interpreting:\n{}\n", content);
    run(content);
}

fn run_prompt() {
    let stdin = io::stdin();

    print!("> ");
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let mut line = line.unwrap();
        if line.is_empty() { continue }
        if !line.ends_with(';') {
            line.push(';');
        }
        run(line);
        print!("\n> ");
        io::stdout().flush().unwrap();
    }
}

fn run(source: String) {
    let parser = Parser::new(source.clone());
    let program = parser.get_ast();
    if let Err((_statements, e)) = program {
        let err = locate_error(&source, e);
        println!("\n{}", format_error(err));
    } else {
        for stmt in program.unwrap().iter() {
            println!("{}", stmt);
            println!("{:?}", stmt);
            println!();
        }
    }
}

fn locate_error(source: &str, err: Error) -> ErrorLocation {
    let pos = err.0.1.pos;
    let len = err.0.1.len;
    let t = Tokenizer::new(source[pos..pos+len].into()).next().typ;
    assert_eq!(t, err.0.1.typ);
    let mut line = 0;
    let mut col = 0;
    for (k, c) in source.char_indices() {
        col += 1;
        if c == '\n' {
            line += 1;
            col = 0;
        }
        if err.0.1.pos <= k {
            break
        }
    }
    ErrorLocation { source: source.to_owned(), err, line, col }
}

fn format_error(err: ErrorLocation) -> String {
    let ErrorLocation { source, err, line, col } = err;
    let Error((expected, token)) = err;

    let left = source[..token.pos].trim_end().rfind('\n').unwrap_or(0);
    let right = source[token.pos..].find('\n').unwrap_or(source.len());
    let context = &source[left..left+right+2];
    format!("Error found in line {line}, column {col}. Error: Expected {expected}, got {token}! Context: \n\n{context}\n")
}

// impl Expr {
//     pub fn eval(&self) -> i64 {
//         match self {
//             Expr::Integer(num)  => *num,
//             Expr::Binary(b)     => b.eval(),
//             Expr::Grouping(b)   => b.as_ref().eval(),
//             Expr::Error(_)    => panic!(),
//             Expr::Function(_)   => todo!(),
//             Expr::Unary(val)    => val.eval(),
//             Expr::String(_)     => todo!(),
//             Expr::Identifier(_) => todo!(),
//             Expr::Bool(val)     => *val as i64,
//             Expr::Block(_)      => todo!(),
//             Expr::If(_)         => todo!(),
//             Expr::FunctionCall(_) => todo!(),
//         }
//     }
// }

// impl BinaryExpr {
//     pub fn eval(&self) -> i64 {
//         let left = self.left.eval();
//         let right = self.right.eval();
//         match self.op {
//             BinaryType::Minus        => left - right,
//             BinaryType::Plus         => left + right,
//             BinaryType::Slash        => left / right,
//             BinaryType::Star         => left * right,
//             BinaryType::GreaterEqual => (left >= right).into(),
//             BinaryType::Greater      => (left > right).into(),
//             BinaryType::LessEqual    => (left <= right).into(),
//             BinaryType::Less         => (left < right).into(),
//             BinaryType::BangEqual    => (left != right).into(),
//             BinaryType::EqualEqual   => (left == right).into(),
//             BinaryType::And          => (left != 0 && right != 0).into(),
//             BinaryType::Or           => (left != 0 || right != 0).into(),
//         }
//     }
// }

// impl UnaryExpr {
//     pub fn eval(&self) -> i64 {
//         match self.op {
//             UnaryType::Minus => - self.val.eval(),
//             UnaryType::Bang => !self.val.eval(),
//         }
//     }
// }


