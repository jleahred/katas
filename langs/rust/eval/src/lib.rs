use crate::program::*;
use idata::cont::IVec;
use std::result::Result;

pub mod expr {
    use crate::program::*;
    use idata::cont::IVec;
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

mod program {
    pub(crate) type Program = Vec<ProgItem>;
    pub(crate) type Stack = Vec<f64>;

    #[derive(Debug)]
    pub enum ProgItem {
        Push(f64),
        Op(Operator),
    }

    #[derive(Debug)]
    pub enum Operator {
        Add,
        Subs,
        Mult,
        Div,
    }
}

pub fn run(p: Program) -> Result<f64, String> {
    let run_op = |s: Stack, op: &Operator| -> Result<Stack, String> {
        let (or, s) = s.ipop();
        let (ol, s) = s.ipop();
        match (ol, or) {
            (Some(l), Some(r)) => match *op {
                Operator::Add => Ok(s.ipush(l + r)),
                Operator::Subs => Ok(s.ipush(l - r)),
                Operator::Mult => Ok(s.ipush(l * r)),
                Operator::Div => Ok(s.ipush(l / r)),
            },
            _ => Err("Incorrect program, empty stack on oper 2 funct".to_string()),
        }
    };
    let final_stack = p.iter().try_fold(vec![], |s, pi| match pi {
        ProgItem::Push(v) => Ok(s.ipush(*v)),
        ProgItem::Op(op) => run_op(s, op),
    })?;
    match final_stack.ipop() {
        (Some(r), fs) => {
            if fs.is_empty() {
                Ok(r)
            } else {
                Err("Incorrect program, not empty stack at end".to_string())
            }
        }
        _ => Err("Incorrect program, not result at end".to_string()),
    }
}

#[test]
fn test_prog() {
    let prog = vec![];
    let prog = prog.ipush(ProgItem::Push(1.0));
    let prog = prog.ipush(ProgItem::Push(2.0));
    let prog = prog.ipush(ProgItem::Push(3.0));
    let prog = prog.ipush(ProgItem::Op(Operator::Mult));
    let prog = prog.ipush(ProgItem::Op(Operator::Add));

    assert!(run(prog) == Ok(7.0))
}

#[test]
fn test_sum() {
    assert!(run(expr::compile("1+2").unwrap()) == Ok(3.0));
    assert!(run(expr::compile("+1+2").unwrap()) == Ok(3.0));
    assert!(run(expr::compile("2-1").unwrap()) == Ok(1.0));
    assert!(run(expr::compile("1-2+3").unwrap()) == Ok(2.0));
    assert!(run(expr::compile("1-2+3-5").unwrap()) == Ok(-3.0));
}

#[test]
fn test_fact() {
    assert!(run(expr::compile("1*2").unwrap()) == Ok(2.0));
    assert!(run(expr::compile("2*3").unwrap()) == Ok(6.0));
    assert!(run(expr::compile("2*3*4").unwrap()) == Ok(24.0));
    assert!(run(expr::compile("-2*3*4").unwrap()) == Ok(-24.0));
}

#[test]
fn test_sum_fact() {
    assert!(run(expr::compile("1+2*3").unwrap()) == Ok(7.0));
    assert!(run(expr::compile("2*3+1").unwrap()) == Ok(7.0));
    assert!(run(expr::compile("2*3-1").unwrap()) == Ok(5.0));
    assert!(run(expr::compile("-1*2+3").unwrap()) == Ok(1.0));
    assert!(run(expr::compile("-1*2+3*2").unwrap()) == Ok(4.0));
    assert!(run(expr::compile("-1*2+3*2+1+1").unwrap()) == Ok(6.0));
    assert!(run(expr::compile("-1*2+3*2+1+1-1").unwrap()) == Ok(5.0));
}

#[test]
fn test_parenth() {
    assert!(run(expr::compile("(1)").unwrap()) == Ok(1.0));
    assert!(run(expr::compile("(+1)").unwrap()) == Ok(1.0));
    assert!(run(expr::compile("(1+2*3)").unwrap()) == Ok(7.0));
    assert!(run(expr::compile("(2*3+1)").unwrap()) == Ok(7.0));
    assert!(run(expr::compile("(2*3-1)").unwrap()) == Ok(5.0));
    assert!(run(expr::compile("(-1*2+3)").unwrap()) == Ok(1.0));
    assert!(run(expr::compile("(-1*2+3*2)").unwrap()) == Ok(4.0));
    assert!(run(expr::compile("1+(2*3)").unwrap()) == Ok(7.0));
    assert!(run(expr::compile("1+(+2*3)").unwrap()) == Ok(7.0));
    assert!(run(expr::compile("1+(-2*3)").unwrap()) == Ok(-5.0));
    assert!(run(expr::compile("(1+2)*3").unwrap()) == Ok(9.0));
    assert!(run(expr::compile("+(1)").unwrap()) == Ok(1.0));
    assert!(run(expr::compile("+(+1)").unwrap()) == Ok(1.0));
    assert!(run(expr::compile("-(-1)").unwrap()) == Ok(1.0));
    assert!(run(expr::compile("+(-1)").unwrap()) == Ok(-1.0));
    assert!(run(expr::compile("-(+1)").unwrap()) == Ok(-1.0));
    assert!(run(expr::compile("-(1+2)*3").unwrap()) == Ok(-9.0));
    assert!(run(expr::compile("+(1+2)*3").unwrap()) == Ok(9.0));
    assert!(run(expr::compile("+(1+2)*(-3)").unwrap()) == Ok(-9.0));
    assert!(run(expr::compile("+(1+(-2))*(-3)").unwrap()) == Ok(3.0));
    assert!(run(expr::compile("+(1+(-2*3))*(-3)").unwrap()) == Ok(15.0));
    assert!(run(expr::compile("+(1+(+2*3))*(-3)").unwrap()) == Ok(-21.0));
    assert!(run(expr::compile("+(1+(2*3))*(-3)").unwrap()) == Ok(-21.0));
}

#[test]
fn test_incorrect() {
    assert!(expr::compile("(1").is_err());
    assert!(expr::compile("1)").is_err());
    assert!(expr::compile("((1)").is_err());
    assert!(expr::compile("(1))").is_err());
    assert!(expr::compile("+-(+1)").is_err());
    assert!(expr::compile("+*1").is_err());
    assert!(expr::compile("-*1").is_err());
    assert!(expr::compile("-/1").is_err());
    assert!(expr::compile("*1").is_err());
    assert!(expr::compile("**1").is_err());
    assert!(expr::compile("*(1)").is_err());
    assert!(expr::compile("+*(1)").is_err());
    assert!(expr::compile("+(*1)").is_err());
    assert!(expr::compile("+(*1)").is_err());
}

#[test]
fn test_spaces() {
    assert!(run(expr::compile(" 1").unwrap()) == Ok(1.0));
    assert!(run(expr::compile("1 ").unwrap()) == Ok(1.0));
    assert!(run(expr::compile(" 1 ").unwrap()) == Ok(1.0));
    assert!(run(expr::compile("  1  ").unwrap()) == Ok(1.0));
    assert!(run(expr::compile(" 1 * 2 ").unwrap()) == Ok(2.0));
    assert!(run(expr::compile("  2  *  3  ").unwrap()) == Ok(6.0));
    assert!(run(expr::compile(" 2  *   3* 4").unwrap()) == Ok(24.0));
    assert!(run(expr::compile("- 2 * 3 * 4 ").unwrap()) == Ok(-24.0));
    assert!(run(expr::compile(" 1 + 2 * 3 ").unwrap()) == Ok(7.0));
    assert!(run(expr::compile("(1)").unwrap()) == Ok(1.0));
    assert!(run(expr::compile(" ( 1 ) ").unwrap()) == Ok(1.0));
    assert!(run(expr::compile(" ( + 1 ) ").unwrap()) == Ok(1.0));
    assert!(run(expr::compile(" (  1+2*3  )  ").unwrap()) == Ok(7.0));
}
