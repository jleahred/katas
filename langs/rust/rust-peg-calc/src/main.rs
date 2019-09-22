use idata::cont::IVec;
use peg::parser;

#[derive(Clone, Copy, Debug)]
enum Command {
    PushVal(f64),
    Fun(Function),
}

#[derive(Clone, Copy, Debug)]
enum Function {
    Add,
    Subs,
    Mult,
    Div,
}

type Stack = Vec<f64>;

#[derive(Default, Debug)]
pub struct Program {
    commands: Vec<Command>,
    // stack: Stack,
}

impl Program {
    fn new() -> Self {
        Default::default()
    }
}

impl Program {
    fn add_cmd(mut self, command: Command) -> Program {
        self.commands.push(command);
        self
    }

    fn add_program(mut self, mut program: Program) -> Self {
        self.commands.append(&mut program.commands);
        self
    }

    fn execute(&self) -> Result<f64, String> {
        let final_stack = self
            .commands
            .iter()
            .try_fold(vec![], |stack, c| exec_command(*c, stack))?;

        if final_stack.len() != 1 {
            Err("Incorrect program!!! end of program with too many elements on stack".to_string())
        } else {
            Ok(*final_stack
                .last()
                .ok_or("Incorrect program!!! final program with empty stack")?)
        }
    }

    fn add_snumber(self, n: &str) -> Result<Program, &'static str> {
        // match n.parse::<f64>() {
        //     Ok(number) => Ok(Program::new().add_cmd(Command::PushVal(number))),
        //     _ => Err("failed parsing number")
        // }
        n.parse()
            .and_then(|number| Ok(self.add_cmd(Command::PushVal(number))))
            .or_else(|_| Err("failed parsing number"))
    }
    fn add_number(self, n: f64) -> Program {
        self.add_cmd(Command::PushVal(n))
    }
    fn add_oper(self, oper: Function) -> Program {
        self.add_cmd(Command::Fun(oper))
    }
    fn mult_minus1(self) -> Program {
        self.add_number(-1.0).add_oper(Function::Mult)
    }
    fn oper_program(self, r: Program, op: Function) -> Program {
        self.add_program(r).add_oper(op)
    }
}

fn exec_command(command: Command, mut stack: Stack) -> Result<Stack, String> {
    Ok(match command {
        Command::PushVal(value) => stack.ipush(value),
        Command::Fun(op) => {
            let (f, s) = match (stack.pop(), stack.pop()) {
                (None, _) => Err("exec command with empty stack"),
                (_, None) => Err("exec command with empty stack"),
                (Some(f), Some(s)) => Ok((f, s)),
            }?;
            let r = match op {
                Function::Add => s + f,
                Function::Subs => s - f,
                Function::Mult => s * f,
                Function::Div => s / f,
            };

            stack.ipush(r)
        }
    })
}

#[cfg(test)]
fn cmp_float(l: f64, r: f64) -> Option<std::cmp::Ordering> {
    if (l, r) == (0.0, 0.0) {
        Some(std::cmp::Ordering::Equal)
    } else if l == 0.0 || r == 0.0 {
        l.partial_cmp(&r)
    } else {
        (l / r - 1.0).partial_cmp(&0.0)
    }
}

#[cfg(test)]
fn eq_float(l: f64, r: f64) -> bool {
    match cmp_float(l, r) {
        Some(std::cmp::Ordering::Equal) => true,
        _ => false,
    }
}

#[test]
pub fn test_machine() {
    let program = Program::new()
        .add_cmd(Command::PushVal(1.0))
        .add_cmd(Command::PushVal(4.0))
        .add_cmd(Command::Fun(Function::Add));
    assert!(eq_float(program.execute().unwrap(), 5.0f64));

    let program = Program::new()
        .add_cmd(Command::PushVal(1.0))
        .add_cmd(Command::PushVal(2.0))
        .add_cmd(Command::PushVal(3.0))
        .add_cmd(Command::Fun(Function::Mult))
        .add_cmd(Command::Fun(Function::Add));
    assert!(eq_float(program.execute().unwrap(), 7.0f64));

    let program = Program::new()
        .add_cmd(Command::PushVal(8.0))
        .add_cmd(Command::PushVal(2.0))
        .add_cmd(Command::Fun(Function::Div));
    assert!(eq_float(program.execute().unwrap(), 4.0f64));
    assert!(eq_float(program.execute().unwrap(), 4.0f64));
}

// fn prog_from_term(l: Program, r: Program, op: &str) -> Program {
//     let operation = match op {
//         "+" => Function::Add,
//         "-" => Function::Subs,
//         _ => unreachable!(),
//     };
//     Program::new()
//         .add_prog(l)
//         .add_prog(r)
//         .add_cmd(Command::Fun(operation))
// }

// fn oper2programs(l: Program, r: Program, op: Function) -> Program {
//     l.add_program(r).add_oper(op)
// }

parser! {
    grammar math_parser() for str {
        pub  rule  compile() -> Program
            =  expr()

        pub  rule  expr() -> Program
            = _() r:(
                  "+" _() "("  _()  e:expr()  _() ")"  _()  { e }
                / "-" _() "("  _()  e:expr()  _() ")"  _()  { e.mult_minus1() }
                /         "("  _()  e:expr()  _() ")"  _()  { e }

                / "+"     t:term()                { t }
                / "-"     t:term()                { t.mult_minus1() }
                /           term()
                ) { r }


        rule no_sign_expr() -> Program
            = _() r:(
                  "("  _()  e:expr()  _() ")"  _()  { e }
                /             term()
                ) { r }

        rule term() -> Program
            =   spc()
                r:( l:factor() spc() "+" spc() r:term()  { l.oper_program(r, Function::Add) }
                /   l:factor() spc() "-" spc() s:factor() "+" spc() r:term()   
                        { l.oper_program(s, Function::Subs).oper_program(r, Function::Add) }
                /   l:factor() spc() "-" spc() s:factor()  { l.oper_program(s, Function::Subs) }
                /     factor()
                )   { r }

        rule factor() -> Program
            =   spc()
                r:( l:number() spc() "*" spc() r:factor()  { l.oper_program(r, Function::Mult) }
                /   l:number() spc() "/" spc() r:factor()  { l.oper_program(r, Function::Div) }
                /     number()
                )   { r }


        //  number      ------------------------
        rule number() -> Program
            = n:$(['0'..='9']+("." ['0'..='9']+)?)    {? Program::add_snumber(Program::new(), n) }

        //  spaces      ------------------------
        rule _()  = quiet!{[' ' | '\t' | '\n' | '\r']*}
        rule spc()  = quiet!{" "*}
    }
}

pub fn main() {
    println!("{:#?}", math_parser::compile("+1-2+3").unwrap())
}

#[test]
pub fn test_1() {
    assert_eq!(math_parser::compile("1").unwrap().execute(), Ok(1.0));
    assert_eq!(math_parser::compile("123").unwrap().execute(), Ok(123.0));
    assert_eq!(
        math_parser::compile("123.12").unwrap().execute(),
        Ok(123.12)
    );
    assert_eq!(
        math_parser::compile("+123.12").unwrap().execute(),
        Ok(123.12)
    );
    assert_eq!(
        math_parser::compile("-123.12").unwrap().execute(),
        Ok(-123.12)
    );
    assert_eq!(
        math_parser::compile("- 123.12").unwrap().execute(),
        Ok(-123.12)
    );
    assert_eq!(math_parser::compile("1+2").unwrap().execute(), Ok(3.0));
    assert_eq!(math_parser::compile("1-2").unwrap().execute(), Ok(-1.0));
    assert_eq!(math_parser::compile("+ 1 - 2").unwrap().execute(), Ok(-1.0));
    assert_eq!(math_parser::compile("(1)").unwrap().execute(), Ok(1.0));
    assert_eq!(math_parser::compile("((1))").unwrap().execute(), Ok(1.0));
    assert_eq!(math_parser::compile("+1+2+3").unwrap().execute(), Ok(6.0));
    assert_eq!(math_parser::compile("+1-2+3").unwrap().execute(), Ok(2.0));
    // assert_eq!(
    //     math_parser::compile(" (  1 + (2 + 3) ) ")
    //         .unwrap()
    //         .execute(),
    //     Ok(6.0)
    // );
}

#[test]
pub fn test_2() {
    // assert_eq!(math_parser::expr("+(1)"), Ok(1.0));
    // assert_eq!(math_parser::expr("+(+1)"), Ok(1.0));
    // assert!(math_parser::expr("++(1)").is_err());
    // assert!(math_parser::expr(" + + (1)").is_err());
    // assert!(math_parser::expr("(++1)").is_err());
    // assert!(math_parser::expr("+1 + +2").is_err());

    // assert_eq!(math_parser::expr("3*2"), Ok(6.0));
    // assert_eq!(math_parser::expr("8/2"), Ok(4.0));
    // assert_eq!(math_parser::expr("+ 8 / 2"), Ok(4.0));
    // assert_eq!(math_parser::expr("- 8 / 2"), Ok(-4.0));

    // assert!(math_parser::expr("- +8 / 2").is_err());
    // assert!(math_parser::expr("- 8 / -2").is_err());

    // assert_eq!(math_parser::expr("1+2*3"), Ok(7.0));
    // assert_eq!(math_parser::expr("1+(2*3)"), Ok(7.0));
    // assert_eq!(math_parser::expr("(1+2)*3"), Ok(9.0));
}
