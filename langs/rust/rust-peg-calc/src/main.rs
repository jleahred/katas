use idata::cont::IVec;
use peg::parser;

#[derive(Clone, Copy)]
enum Command {
    Push(f64),
    Oper(Operation),
}

#[derive(Clone, Copy)]
enum Operation {
    Add,
    Subs,
    Mult,
    Div,
}

type Stack = Vec<f64>;

#[derive(Default)]
struct Program {
    commands: Vec<Command>,
    stack: Stack,
}

impl Program {
    fn new() -> Self {
        Default::default()
    }
}

impl Program {
    fn add_command(mut self, command: Command) -> Program {
        self.commands.push(command);
        self
    }
    fn execute(self) -> Result<f64, String> {
        let final_stack = self
            .commands
            .iter()
            .try_fold(self.stack, |stack, c| exec_command(*c, stack))?;

        if final_stack.len() != 1 {
            Err("end of program with too many elements on stack".to_string())
        } else {
            Ok(*final_stack.last().ok_or("final program with empty stack")?)
        }
    }
}

fn exec_command(command: Command, mut stack: Stack) -> Result<Stack, String> {
    Ok(match command {
        Command::Push(value) => stack.ipush(value),
        Command::Oper(op) => {
            let (f, s) = match (stack.pop(), stack.pop()) {
                (None, _) => Err("exec command with empty stack"),
                (_, None) => Err("exec command with empty stack"),
                (Some(f), Some(s)) => Ok((f, s)),
            }?;
            let r = match op {
                Operation::Add => s + f,
                Operation::Subs => s - f,
                Operation::Mult => s * f,
                Operation::Div => s / f,
            };

            stack.ipush(r)
        }
    })
}

fn cmp_float(l: f64, r: f64) -> Option<std::cmp::Ordering> {
    if (l, r) == (0.0, 0.0) {
        Some(std::cmp::Ordering::Equal)
    } else if l == 0.0 || r == 0.0 {
        l.partial_cmp(&r)
    } else {
        (l / r - 1.0).partial_cmp(&0.0)
    }
}

fn eq_float(l: f64, r: f64) -> bool {
    match cmp_float(l, r) {
        Some(std::cmp::Ordering::Equal) => true,
        _ => false,
    }
}

#[test]
pub fn test_machine() {
    let program = Program::new()
        .add_command(Command::Push(1.0))
        .add_command(Command::Push(4.0))
        .add_command(Command::Oper(Operation::Add));
    assert!(eq_float(program.execute().unwrap(), 5.0f64));

    let program = Program::new()
        .add_command(Command::Push(1.0))
        .add_command(Command::Push(2.0))
        .add_command(Command::Push(3.0))
        .add_command(Command::Oper(Operation::Mult))
        .add_command(Command::Oper(Operation::Add));
    assert!(eq_float(program.execute().unwrap(), 7.0f64));

    let program = Program::new()
        .add_command(Command::Push(8.0))
        .add_command(Command::Push(2.0))
        .add_command(Command::Oper(Operation::Div));
    assert!(eq_float(program.execute().unwrap(), 4.0f64));
}

parser! {
    grammar math_parser() for str {
        pub rule expr() -> f64
            = number()
        // pub rule expr() -> f64
        //     = _() r:(
        //           "+" _() "("  _()  e:expr()  _() ")"  _()  { e }
        //         / "-" _() "("  _()  e:expr()  _() ")"  _()  { -1.0 * e }
        //         /         "("  _()  e:expr()  _() ")"  _()  { e }

        //         / "+"     t:term()                { t }
        //         / "-"     t:term()                { -1.0 * t }
        //         /           term()
        //         ) { r }


        // rule no_sign_expr() -> f64
        //     = _() r:(
        //           "("  _()  e:expr()  _() ")"  _()  { e }
        //         /             term()
        //         ) { r }

        // rule term() -> f64
        //     =   spc()
        //         r:( l:factor() spc() "+" spc() r:term()  { l + r }
        //         /   l:factor() spc() "-" spc() r:term()  { l - r }
        //         /     factor()
        //         )   { r }

        // rule factor() -> f64
        //     =   spc()
        //         r:( l:number() spc() "*" spc() r:factor()  { l * r }
        //         /   l:number() spc() "/" spc() r:factor()  { l / r }
        //         /     number()
        //         )   { r }


        //  number      ------------------------
        rule number() -> f64
            = n:$(['0'..='9']+("." ['0'..='9']+)?)
                {? n.parse().or_else(|_| Err("failed parsing number")) }

        //  spaces      ------------------------
        rule _()  = quiet!{[' ' | '\t' | '\n' | '\r']*}
        rule spc()  = quiet!{" "*}
    }
}

pub fn main() {}

#[test]
pub fn test_1() {
    assert_eq!(math_parser::expr("1"), Ok(1.0));
    // assert_eq!(math_parser::expr("123"), Ok(123.0));
    // assert_eq!(math_parser::expr("123.12"), Ok(123.12));
    // assert_eq!(math_parser::expr("+123.12"), Ok(123.12));
    // assert_eq!(math_parser::expr("-123.12"), Ok(-123.12));
    // assert_eq!(math_parser::expr("- 123.12"), Ok(-123.12));
    // assert_eq!(math_parser::expr("1+2"), Ok(3.0));
    // assert_eq!(math_parser::expr("1-2"), Ok(-1.0));
    // assert_eq!(math_parser::expr("+ 1 - 2"), Ok(-1.0));
    // assert_eq!(math_parser::expr("(1)"), Ok(1.0));
    // assert_eq!(math_parser::expr("((1))"), Ok(1.0));
    // assert_eq!(math_parser::expr("+1+2+3"), Ok(6.0));
    // assert_eq!(math_parser::expr("+1-2+3"), Ok(2.0));
    // assert_eq!(math_parser::expr(" (  1 + (2 + 3) ) "), Ok(6.0));
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
