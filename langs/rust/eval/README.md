# Eval

A simple algebraic expression evaluator

It's a classic text parsing exercise.

It's written in Rust with the fantastic lib _rust-peg_

There are better ways to solve this with this lib, but this is an exercise to know and practice.

The program will parse the text string, generating a program (compilation) to be executed by the _run_ function.

The run function executes the following instructions on a stack machine.

```rust
pub enum ProgItem {
    Push(f64),
    Op(Operator),
}
```

The possible operations are:

```rust
pub enum Operator {
    Add,
    Subs,
    Mult,
    Div
}
```

When compiling a text, a vector of instructions will be generated for the program

```rust
let txt_expr = "1+2*3";
let compiled = eval::expr::compile(txt_expr);
```

A simple full program to evaluate an expression...

```rust
extern crate eval;

fn main() {
    let txt_expr = "1+2*3";
    let compiled = eval::expr::compile(txt_expr);
    println!("{:?}", compiled);
    println!("{:?}", eval::run(compiled.unwrap()));
}
```

It will produce...

Compiled program:

```
Ok([Push(1.0), Push(2.0), Push(3.0), Op(Mult), Op(Add)])
```

And after running the program:

```
Ok(7.0)
```

## Acepted inputs

```rust
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
```

incorrect inputs...

```rust
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
```
