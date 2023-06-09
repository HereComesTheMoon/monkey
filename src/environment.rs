use std::collections::HashMap;
use crate::parser::{Expr, Statement, BinaryExpr, BinaryType, UnaryType, UnaryExpr, IfExpr, BlockExpr};

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

struct Env(Vec<HashMap<String, Object>>);

impl Env {
    fn get(&self, name: &str) -> Option<&Object> {
        for scope in self.0.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val)
            }
        }
        None
    }

    fn drop_scope(&mut self) {
        self.0.pop().unwrap();
    }
}

trait Eval {
    fn eval(&self) -> Result<Object, IErr>;
}

impl Eval for Expr {
    fn eval(&self) -> Result<Object, IErr> {
        match self {
            Expr::Binary(val)       => val.eval(),
            Expr::Block(val)        => val.eval(),
            Expr::Bool(val)         => Ok(Object::Boolean(*val)),
            Expr::Error(val)        => todo!(),
            Expr::Function(val)     => todo!(),
            Expr::Grouping(val)     => val.as_ref().eval(),
            Expr::Identifier(val)   => todo!(),
            Expr::If(val)           => val.eval(),
            Expr::Integer(val)      => Ok(Object::Integer(*val)),
            Expr::String(val)       => Ok(Object::String(val.clone())),
            Expr::Unary(val)        => val.eval(),
            Expr::FunctionCall(val) => todo!(),
        }
    }
}

impl Eval for BinaryExpr {
    fn eval(&self) -> Result<Object, IErr> {
        let left = self.left.eval()?;
        let right = self.right.eval()?;
        match (left, right) {
            (Object::Boolean(val_l), Object::Boolean(val_r)) => {
                match self.op {
                    BinaryType::And        => Ok(Object::Boolean(val_l && val_r)),
                    BinaryType::BangEqual  => Ok(Object::Boolean(val_l != val_r)),
                    BinaryType::EqualEqual => Ok(Object::Boolean(val_l == val_r)),
                    BinaryType::Or         => Ok(Object::Boolean(val_l || val_r)),
                    _                      => todo!(),
                }
            }
            (Object::Integer(val_l), Object::Integer(val_r)) => {
                match self.op {
                    BinaryType::BangEqual    => Ok(Object::Boolean(val_l != val_r)),
                    BinaryType::EqualEqual   => Ok(Object::Boolean(val_l == val_r)),
                    BinaryType::Greater      => Ok(Object::Boolean(val_l > val_r)),
                    BinaryType::GreaterEqual => Ok(Object::Boolean(val_l >= val_r)),
                    BinaryType::Less         => Ok(Object::Boolean(val_l < val_r)),
                    BinaryType::LessEqual    => Ok(Object::Boolean(val_l <= val_r)),
                    BinaryType::Minus        => Ok(Object::Integer(val_l - val_r)),
                    BinaryType::Plus         => Ok(Object::Integer(val_l + val_r)),
                    BinaryType::Slash        => Ok(Object::Integer(val_l / val_r)),
                    BinaryType::Star         => Ok(Object::Integer(val_l * val_r)),
                    _                        => todo!(),
                }
            }
            (Object::String(val_l), Object::String(val_r)) => {
                match self.op {
                    BinaryType::Plus => Ok(Object::String(val_l + &val_r)),
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
}

impl Eval for UnaryExpr {
    fn eval(&self) -> Result<Object, IErr> {
        let inner = self.val.eval()?;
        match (self.op, inner) {
            (UnaryType::Bang , Object::Boolean(val)) => Ok(Object::Boolean(!val)),
            (UnaryType::Minus, Object::Integer(val)) => Ok(Object::Integer(-val)),
            _ => todo!(),
        }
    }
}

impl Eval for IfExpr {
    fn eval(&self) -> Result<Object, IErr> {
        let cond = self.cond.eval()?;
        let cond = match cond {
            Object::Boolean(true)             => true,
            Object::Boolean(false)            => false,
            Object::Integer(0)                => false,
            Object::Integer(_)                => true,
            Object::String(s) if s.is_empty() => false,
            Object::String(_)                 => true,
            Object::Null                      => false,
        };
        if cond {
            return self.cons.eval();
        }
        if self.alt.is_some() {
            return self.alt.as_ref().unwrap().eval();
        }
        Ok(Object::Null)
    }
}

impl Eval for BlockExpr {
    fn eval(&self) -> Result<Object, IErr> {
        let mut last_val = Object::Null;
        for stmt in self.statements.iter() {
            match stmt {
                Statement::Let(val)    => todo!(),
                Statement::Expr(val)   => {
                    last_val = val.val.eval()?;
                }
                Statement::Return(val) => return val.val.eval(),
            }
        }
        Ok(last_val)
    }
}

#[derive(Debug)]
enum IErr {
    
}

// struct BinErr {
    
// }

#[cfg(test)]
mod test {
    use std::{assert_eq, println};

    use super::*;
    use crate::parser::Parser;

    fn eval_expr(source: &str) -> Result<Object, IErr> {
        let mut source = source.to_string();
        source.push(';');
        let parser = Parser::new(source);
        let res = parser.get_ast();
        if res.is_err() {
            println!("PARSING ERROR: {res:?}");
            panic!();
        }
        let res = res.unwrap();
        assert_eq!(res.len(), 1);
        let res = res.first().unwrap();
        match res {
            Statement::Let(_)    => todo!(),
            Statement::Expr(val) => Ok(val.val.eval()?),
            Statement::Return(_) => todo!(),
        }
    }

    fn eval_expr_test(source: &str, wanted: Result<Object, IErr>) {
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

        eval_expr_test("1 < 2", Ok(Object::Boolean(true)));
        eval_expr_test("1 <= 2", Ok(Object::Boolean(true)));
        eval_expr_test("1 >= 2", Ok(Object::Boolean(false)));
        eval_expr_test("1 == 2", Ok(Object::Boolean(false)));
        eval_expr_test("1 != 2", Ok(Object::Boolean(true)));
        eval_expr_test("2 == 2", Ok(Object::Boolean(true)));
        eval_expr_test("-1 <= 2 - 3", Ok(Object::Boolean(true)));
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

    #[test]
    fn test_if_eval() {
        eval_expr_test("if (true) { 123; }", Ok(Object::Integer(123)));
        eval_expr_test("if (true) { 1; 2; 3; return 4; }", Ok(Object::Integer(4)));
        eval_expr_test("if (false) { 1; 2; 3; return 4; }", Ok(Object::Null));
        eval_expr_test("if (false) {} else { 1; }", Ok(Object::Integer(1)));
    }
}
