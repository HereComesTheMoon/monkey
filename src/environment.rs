use std::collections::HashMap;
use crate::parser::{Expr, Statement, BinaryExpr, BinaryType, UnaryType, UnaryExpr};

struct Program {
    prg: Vec<Statement>,
    env: Vec<HashMap<String, Expr>>,
}

impl Program {
    pub fn new(program: Vec<Statement>) -> Self {
        Program { prg: program, env: vec![HashMap::new()] }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Object {
    Boolean(bool),
    Integer(i64),
    String(String),
    Null,
}

impl Expr {
    fn eval(&self) -> Object {
        match self {
            Expr::Binary(val)       => val.eval(),
            Expr::Block(val)        => todo!(),
            Expr::Bool(val)         => Object::Boolean(*val),
            Expr::Error(val)        => todo!(),
            Expr::Function(val)     => todo!(),
            Expr::Grouping(val)     => val.as_ref().eval(),
            Expr::Identifier(val)   => todo!(),
            Expr::If(val)           => todo!(),
            Expr::Integer(val)      => Object::Integer(*val),
            Expr::String(val)       => Object::String(val.clone()),
            Expr::Unary(val)        => val.eval(),
            Expr::FunctionCall(val) => todo!(),
        }
    }
}

impl BinaryExpr {
    fn eval(&self) -> Object {
        let left = self.left.eval();
        let right = self.right.eval();
        match (left, right) {
            (Object::Boolean(val_l), Object::Boolean(val_r)) => {
                match self.op {
                    BinaryType::And        => Object::Boolean(val_l && val_r),
                    BinaryType::BangEqual  => Object::Boolean(val_l != val_r),
                    BinaryType::EqualEqual => Object::Boolean(val_l == val_r),
                    BinaryType::Or         => Object::Boolean(val_l || val_r),
                    _                      => todo!(),
                }
            }
            (Object::Integer(val_l), Object::Integer(val_r)) => {
                match self.op {
                    BinaryType::BangEqual    => Object::Boolean(val_l != val_r),
                    BinaryType::EqualEqual   => Object::Boolean(val_l == val_r),
                    BinaryType::Greater      => Object::Boolean(val_l > val_r),
                    BinaryType::GreaterEqual => Object::Boolean(val_l >= val_r),
                    BinaryType::Less         => Object::Boolean(val_l < val_r),
                    BinaryType::LessEqual    => Object::Boolean(val_l <= val_r),
                    BinaryType::Minus        => Object::Integer(val_l - val_r),
                    BinaryType::Plus         => Object::Integer(val_l + val_r),
                    BinaryType::Slash        => Object::Integer(val_l / val_r),
                    BinaryType::Star         => Object::Integer(val_l * val_r),
                    _                        => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}

impl UnaryExpr {
    fn eval(&self) -> Object {
        let inner = self.val.eval();
        match (self.op, inner) {
            (UnaryType::Bang , Object::Boolean(val)) => Object::Boolean(!val),
            (UnaryType::Minus, Object::Integer(val)) => Object::Integer(-val),
            _ => todo!(),
        }
    }
}

// enum IErr {
    
// }

// struct BinErr {
    
// }

#[cfg(test)]
mod test {
    use std::{assert_eq, println};

    use super::*;
    use crate::{parser::Parser, errors::Error};

    fn eval_expr(source: &str) -> Result<Object, Error> {
        let mut source = source.to_string();
        source.push(';');
        let parser = Parser::new(source);
        let res = parser.get_ast();
        if res.is_err() {
            return Err(res.unwrap_err().1)
        }
        let res = res.unwrap();
        assert_eq!(res.len(), 1);
        let res = res.first().unwrap();
        match res {
            Statement::Let(_)    => todo!(),
            Statement::Expr(val) => Ok(val.val.eval()),
            Statement::Return(_) => todo!(),
        }
    }

    fn eval_expr_test(source: &str, wanted: Result<Object, ()>) {
        let res = eval_expr(source);
        println!("SOURCE: {source}");
        println!("WANTED: {wanted:?}. GOT: {res:?}");
        if wanted.is_err() {
            assert!(res.is_err());
            return
        }

        let res = res.unwrap();
        let wanted = wanted.unwrap();
        assert_eq!(wanted, res);
    }

    #[test]
    fn test_bool_eval() {
        eval_expr_test("true", Ok(Object::Boolean(true)));
        eval_expr_test("false", Ok(Object::Boolean(false)));
        eval_expr_test("true and true", Ok(Object::Boolean(true)));
        eval_expr_test("true or false", Ok(Object::Boolean(true)));
        eval_expr_test("!false", Ok(Object::Boolean(true)));
        eval_expr_test("!!!false", Ok(Object::Boolean(true)));
        eval_expr_test("true != false", Ok(Object::Boolean(true)));
        eval_expr_test("false == true", Ok(Object::Boolean(false)));
    }

    #[test]
    fn test_int_eval() {
        eval_expr_test("0", Ok(Object::Integer(0)));
        eval_expr_test("1", Ok(Object::Integer(1)));
        eval_expr_test("1 + 2 + 3", Ok(Object::Integer(6)));
        eval_expr_test("1 * 2 + 3", Ok(Object::Integer(5)));
        eval_expr_test("1 + 2 * 3", Ok(Object::Integer(7)));
        eval_expr_test("3 / 2", Ok(Object::Integer(1)));
        eval_expr_test("-1 / 2", Ok(Object::Integer(0)));
        eval_expr_test("-6 * - 6", Ok(Object::Integer(36)));
    }
}
