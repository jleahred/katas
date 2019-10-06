use idata::cont::IVec;
use peg::parser;

pub fn main() {
    let program = math_parser::compile("-2*3*4+1").unwrap();
    println!("{:#?}", program);
    println!("{:#?}", program.execute());
}

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
    // pc
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

parser! {
    grammar math_parser() for str {
        pub  rule  compile() -> Program
            =  expr()

        rule  expr() -> Program
            = _() r:(
                      "-"    spc()  l:factor() spc()  t:sub_terms()*
                                    { t.into_iter().fold(
                                        Program::new().add_number(0.0).add_program(l).add_oper(Function::Subs),
                                        | acc , prg | { acc.add_program(prg)}) }
                    / "+"?   spc()  t:term()              { t }
                ) { r }

        rule term() -> Program
            =   spc()  l:factor() spc()  t:sub_terms()*
                                    { t.into_iter().fold(l, | acc , prg | { acc.add_program(prg)}) }

        rule sub_terms() -> Program
            =   "+" spc() r:factor()  { r.add_oper(Function::Add) }
            /   "-" spc() r:factor()  { r.add_oper(Function::Subs) }

        rule factor() -> Program
            =   spc()  l:numb_or_parenth() spc()  t:sub_factors()*
                                    { t.into_iter().fold(l, |acc , prg | { acc.add_program(prg) }) }

        rule sub_factors() -> Program
            =   "*" spc() r:numb_or_parenth()  { r.add_oper(Function::Mult) }
            /   "/" spc() r:numb_or_parenth()  { r.add_oper(Function::Div) }


        rule parenth() -> Program
            = _() "("  _()  e:expr()  _() ")"  _()  { e }

        rule numb_or_parenth() -> Program
            = (number() / parenth())

        //  number      ------------------------
        rule number() -> Program
            = n:$(['0'..='9']+("." ['0'..='9']+)?)    {? Program::add_snumber(Program::new(), n) }

        //  spaces      ------------------------
        rule _()  = quiet!{[' ' | '\t' | '\n' | '\r']*}
        rule spc()  = quiet!{" "*}
    }
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
    assert_eq!(math_parser::compile("1-2-3").unwrap().execute(), Ok(-4.0));
    assert_eq!(math_parser::compile("1+2").unwrap().execute(), Ok(3.0));
    assert_eq!(math_parser::compile("1-2").unwrap().execute(), Ok(-1.0));
    assert_eq!(math_parser::compile("+ 1 - 2").unwrap().execute(), Ok(-1.0));
    assert_eq!(math_parser::compile("(1)").unwrap().execute(), Ok(1.0));
    assert_eq!(math_parser::compile("((1))").unwrap().execute(), Ok(1.0));
    assert_eq!(math_parser::compile("+1+2+3").unwrap().execute(), Ok(6.0));
    assert_eq!(math_parser::compile("+1-2+3").unwrap().execute(), Ok(2.0));
    assert_eq!(
        math_parser::compile(" (  1 + (2 + 3) ) ")
            .unwrap()
            .execute(),
        Ok(6.0)
    );
}

#[test]
pub fn test_2() {
    assert_eq!(math_parser::compile("+(1)").unwrap().execute(), Ok(1.0));
    assert_eq!(math_parser::compile("+(+1)").unwrap().execute(), Ok(1.0));
    assert_eq!(math_parser::compile("3*2").unwrap().execute(), Ok(6.0));
    assert_eq!(math_parser::compile("8/2").unwrap().execute(), Ok(4.0));
    assert_eq!(math_parser::compile("+ 8 / 2").unwrap().execute(), Ok(4.0));
    assert_eq!(math_parser::compile("- 8 / 2").unwrap().execute(), Ok(-4.0));
    assert_eq!(math_parser::compile("1+2*3").unwrap().execute(), Ok(7.0));
    assert_eq!(math_parser::compile("1+(2*3)").unwrap().execute(), Ok(7.0));
    assert_eq!(math_parser::compile("(1+2)+3").unwrap().execute(), Ok(6.0));
    assert_eq!(math_parser::compile("1*2+3").unwrap().execute(), Ok(5.0));
    assert_eq!(math_parser::compile("(1+2)*3").unwrap().execute(), Ok(9.0));
    assert_eq!(math_parser::compile("6/3*2").unwrap().execute(), Ok(4.0));
    assert_eq!(
        math_parser::compile("(+(+1)+2)*3").unwrap().execute(),
        Ok(9.0)
    );

    assert_eq!(
        math_parser::compile("(-(+1)+2)*3").unwrap().execute(),
        Ok(-9.0)
    );

    assert_eq!(
        math_parser::compile("(-(+1)+2)*(-3)").unwrap().execute(),
        Ok(9.0)
    );

    assert_eq!(math_parser::compile("2*3*4").unwrap().execute(), Ok(24.0));
    assert_eq!(math_parser::compile("2*3*4+1").unwrap().execute(), Ok(25.0));
    assert_eq!(
        math_parser::compile("-2*3*4+1").unwrap().execute(),
        Ok(-25.0)
    );
}

#[test]
pub fn test_errors() {
    assert!(math_parser::compile("+-1").is_err());
    assert!(math_parser::compile("+*1").is_err());
    assert!(math_parser::compile("(1").is_err());
    assert!(math_parser::compile("1+(2*3))").is_err());
    assert!(math_parser::compile("(1+(2*3)").is_err());
}
