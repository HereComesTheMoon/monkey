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

fn run(program: Vec<Statement>) -> Result<Object, IErr> {
    let outer_scope = BlockExpr { statements: program };
    let mut env = Env::new();
    outer_scope.eval(&mut env)
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Object {
    Boolean(bool),
    Integer(i64),
    String(String),
    Null,
}

struct Env(Vec<HashMap<String, Object>>);

impl Env {
    fn new() -> Env {
        Env(vec![HashMap::new()])
    }

    fn get(&self, name: &str) -> Result<Object, IErr> {
        for scope in self.0.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Ok(val.clone())
            }
        }
        Err(IErr::LookupError)
    }

    fn bind(&mut self, name: &str, value: Object) {
        self.0.last_mut().unwrap().insert(name.to_owned(), value);
    }

    fn open_scope(&mut self) {
        self.0.push(HashMap::new());
    }

    fn drop_scope(&mut self) {
        self.0.pop().unwrap();
    }
}

trait Eval {
    fn eval(&self, env: &mut Env) -> Result<Object, IErr>;
}

impl Eval for Expr {
    fn eval(&self, env: &mut Env) -> Result<Object, IErr> {
        match self {
            Expr::Binary(val)       => val.eval(env),
            Expr::Block(val)        => val.eval(env),
            Expr::Bool(val)         => Ok(Object::Boolean(*val)),
            Expr::Error(val)        => todo!(),
            Expr::Function(val)     => todo!(),
            Expr::Grouping(val)     => val.as_ref().eval(env),
            Expr::Identifier(val)   => env.get(val),
            Expr::If(val)           => val.eval(env),
            Expr::Integer(val)      => Ok(Object::Integer(*val)),
            Expr::String(val)       => Ok(Object::String(val.clone())),
            Expr::Unary(val)        => val.eval(env),
            Expr::FunctionCall(val) => todo!(),
        }
    }
}

impl Eval for BinaryExpr {
    fn eval(&self, env: &mut Env) -> Result<Object, IErr> {
        let left = self.left.eval(env)?;
        let right = self.right.eval(env)?;
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
    fn eval(&self, env: &mut Env) -> Result<Object, IErr> {
        let inner = self.val.eval(env)?;
        match (self.op, inner) {
            (UnaryType::Bang , Object::Boolean(val)) => Ok(Object::Boolean(!val)),
            (UnaryType::Minus, Object::Integer(val)) => Ok(Object::Integer(-val)),
            _ => todo!(),
        }
    }
}

impl Eval for IfExpr {
    fn eval(&self, env: &mut Env) -> Result<Object, IErr> {
        let cond = self.cond.eval(env)?;
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
            return self.cons.eval(env);
        }
        if self.alt.is_some() {
            return self.alt.as_ref().unwrap().eval(env);
        }
        Ok(Object::Null)
    }
}

impl Eval for BlockExpr {
    fn eval(&self, env: &mut Env) -> Result<Object, IErr> {
        env.open_scope();
        let mut last_val = Object::Null;
        for stmt in self.statements.iter() {
            match stmt {
                Statement::Let(val)    => {
                    let value = val.val.eval(env)?;
                    env.bind(&val.name, value);
                }
                Statement::Expr(val)   => {
                    last_val = val.val.eval(env)?;
                }
                Statement::Return(val) => {
                    let res = val.val.eval(env);
                    env.drop_scope();
                    return res;
                },
            }
        }
        env.drop_scope();
        Ok(last_val)
    }
}

#[derive(Debug)]
enum IErr {
    LookupError,
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
        let mut env = Env::new();
        match res {
            Statement::Let(_)    => todo!(),
            Statement::Expr(val) => Ok(val.val.eval(&mut env)?),
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

    fn eval_program(source: &str) -> Result<Object, IErr> {
        let mut source = source.to_string();
        let parser = Parser::new(source);
        let res = parser.get_ast();
        if res.is_err() {
            println!("PARSING ERROR: {res:?}");
            panic!();
        }
        let res = res.unwrap();
        let result = run(res);
        result
    }

    fn eval_program_test(source: &str, wanted: Result<Object, IErr>) {
        let res = eval_program(source);
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

    #[test]
    fn test_binding() {
        eval_program_test("let a = 5;", Ok(Object::Null));
        eval_program_test("let a = 5; a;", Ok(Object::Integer(5)));
        eval_program_test("let a = 5; let b = 7; let c = a + b; c;", Ok(Object::Integer(12)));
        eval_program_test("let a = \"foo\"; let b = \"bar\"; let c = a + b; c;", Ok(Object::String("foobar".into())));
    }

    #[test]
    fn test_scoping() {
        eval_program_test("{ let a = 5; a; };", Ok(Object::Integer(5)));
        eval_program_test("{ let a = 5; }; a;", Err(IErr::LookupError));
        eval_program_test("let a = 5; { let b = 7; a + b; };", Ok(Object::Integer(12)));
        // eval_program_test("let a = \"foo\"; let b = \"bar\"; let c = a + b; c;", Ok(Object::String("foobar".into())));
    }
}
