mod tokenizer;
mod tokens;
mod parser;
mod errors;
use std::env;

use std::fs;
use std::io;
use std::io::{BufRead, Write};
use std::println;
use std::process;
use parser::Parser;
use errors::Error;

use crate::errors::format_error;
use crate::errors::locate_error;

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


