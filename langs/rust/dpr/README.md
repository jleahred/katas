# dpr

Evolution of... [dynparser](https://github.com/jleahred/dynparser)

- [repository](https://github.com/jleahred/dpr)
- [doc](https://docs.rs/dpr/)
- [rust-crate](https://crates.io/crates/dpr)

```txt
  Text -> Parsing -> Transform -> Text
```

More info about the `peg` syntax bellow.

## Usage

Add to `cargo.toml`

```toml
[dependencies]
# dpr = "0.1.0" soon
dpr = {git = "https://github.com/jleahred/dpr" }
```

Watch examples below

## Modifications

```txt
    0.1.0 First version
```

## TODO

* Document readme
* Calculator example
* Adding external functions
* Upload to `crates`

## About

Giveng a `peg` grammar extended, it will verify the input and can generate an output based on transformation rules

But let's see by examples

### Simple example

Starting with this `peg`

Peg:
```text
        main    =   char+
        char    =   'a'     -> A
                /   'b'     -> B
                /   .
```

Given this `input`

Input:
```text
    aaacbbabdef
```

We got as result:

Output:
```text
    AAAcBBABdef
```

Addition example

Peg:
```text
        main    =   expr

        expr    =   num:num                -> PUSH $(num)$(:endl)
                    (op:op  expr:expr)?    -> $(expr)EXEC $(op)$(:endl)

        op      =   '+'     -> ADD
                /   '-'     -> SUB

        num     =   [0-9]+  ('.' [0-9])?
```

Input:
```text
    1+2-3
```

Output:
```text
    PUSH 1
    PUSH 2
    PUSH 3
    EXEC SUB
    EXEC ADD
```

### Execution flow

Basic text trasnformation flow.


```text

   DSL flow


 .--------.
 |  peg   |
 |  user  |
 '--------'
      |
      v
 .--------.
 |  GEN   |
 | rules  |
 '--------'
      |                .----------.
      |                |  input   |
      |                |   user   |
      |                '----------'
      |                      |
      |                      v
      |                .----------.
      |                |  parse   |
      '--------------->|          |
                       '----------'
                             |
                             v
                        .---------.
                        | replace |
                        |         |
                        '---------'
                             |
                             v
                        .--------.
                        | OUTPUT |
                        |        |
                        '--------'


```

The `rust` code for first example...

```rust
extern crate dpr;

fn main() -> Result<(), dpr::Error> {
    let result = dpr::Peg::new(
        "
        main    =   char+
        char    =   'a'     -> A
                /   'b'     -> B
                /   .
    ",
    )
    .gen_rules()?
    .parse("aaacbbabdef")?
    .replace()?
    //  ...
    ;

    println!("{:#?}", result);
    Ok(())
}
```

### Let's see step by step

Creating rules...

```rust
extern crate dpr;

fn main() -> Result<(), dpr::Error> {
    let result = dpr::Peg::new(
        "
        main    =   char+
        char    =   'a'     -> A
                /   'b'     -> B
                /   .
    ",
    )
    .gen_rules()?
    // .parse("aaacbbabdef")?
    // .replace()?
    //  ...
    ;

    println!("{:#?}", result);
    Ok(())
}
```

Produce a set of rules like...

```text
SetOfRules(
    {
        "main": And(
            MultiExpr(
                [
                    Repeat(
                        RepInfo {
                            expression: RuleName(
                                "char",
                            ),
                            min: NRep(
                                1,
                            ),
                            max: None,
                        },
                    ),
                ],
            ),
        ),
        "char": Or(
            MultiExpr(
                [
                    And(
                        MultiExpr(
                            [
                                MetaExpr(
                                    Transf2(
                                        Transf2Expr {
                                            mexpr: MultiExpr(
                                                [
                                                    Simple(
                                                        Literal(
                                                            "a",
                                                        ),
                                                    ),
                                                ],
                                            ),
                                            transf2_rules: "A",
                                        },
                                    ),
                                ),
                            ],
                        ),
                    ),
                    And(
                        MultiExpr(
                            [
                                MetaExpr(
                                    Transf2(
                                        Transf2Expr {
                                            mexpr: MultiExpr(
                                                [
                                                    Simple(
                                                        Literal(
                                                            "b",
                                                        ),
                                                    ),
                                                ],
                                            ),
                                            transf2_rules: "B",
                                        },
                                    ),
                                ),
                            ],
                        ),
                    ),
                    And(
                        MultiExpr(
                            [
                                Simple(
                                    Dot,
                                ),
                            ],
                        ),
                    ),
                ],
            ),
        ),
    },
)
```

This set of rules will let us to `parse` and generate the `AST` for any `input`

Next step, `parsing` the `input` with generated `rules`...

Creating rules...
(With a simplified input in order to reduce the `output` size)

```rust
extern crate dpr;

fn main() -> Result<(), dpr::Error> {
    let result = dpr::Peg::new(
        "
        main    =   char+
        char    =   'a'     -> A
                /   'b'     -> B
                /   .
    ",
    )
    .gen_rules()?
    .parse("acb")?
    // .replace()?
    //  ...
    ;

    println!("{:#?}", result);
    Ok(())
}
```

Now you can see de produced `AST`

```text
Rule(
    (
        "main",
        [
            Rule(
                (
                    "char",
                    [
                        Transf2(
                            (
                                "A",
                                [
                                    Val(
                                        "a",
                                    ),
                                ],
                            ),
                        ),
                    ],
                ),
            ),
            Rule(
                (
                    "char",
                    [
                        Val(
                            "c",
                        ),
                    ],
                ),
            ),
            Rule(
                (
                    "char",
                    [
                        Transf2(
                            (
                                "B",
                                [
                                    Val(
                                        "b",
                                    ),
                                ],
                            ),
                        ),
                    ],
                ),
            ),
        ],
    ),
)
```

And running the transformations...

```rust
extern crate dpr;

fn main() -> Result<(), dpr::Error> {
    let result = dpr::Peg::new(
        "
        main    =   char+
        char    =   'a'     -> A
                /   'b'     -> B
                /   .
    ",
    )
    .gen_rules()?
    .parse("acb")?
    .replace()?
    //  ...
    ;

    println!("{:#?}", result);
    Ok(())
}
```

```txt
"AcB"
```

## PEG rules grammar

You saw some examples, let see in detail

| token      | Description                                                           |
| ---------- | --------------------------------------------------------------------- |
| `=`        | On left, symbol, on right expresion defining symbol                   |
| `symbol`   | It's an string without quotes, no spaces, and ascii                   |
| `.`        | Any char                                                              |
| `"..."`    | Literal delimited by quotes                                           |
| `<space>`  | Separate tokens and Rule concatenation (`and` operation)              |
| `/`        | Or operation                                                          |
| `(...)`    | A expression composed of sub expresions                               |
| `?`        | One optional                                                          |
| `*`        | Repeat 0 or more                                                      |
| `+`        | Repeat 1 or more                                                      |
| `!`        | negate expression, continue if not followed without consume           |
| `[...]`    | Match chars. It's a list or ranges (or both)                          |
| `->`       | after the arrow, we have the transformation rule                      |
| `:`        | To give a name, in order to use later in transformation               |
| error(...) | This let's you to define an error message when this rule is satisfied |

Below there is the `grammar` witch define the valid `peg` inputs.
BTW, this `grammar` has been parsed to generate the code to parse itself ;-)

Let's see by example

### Rules by example

A simple literal string.

```peg
main = "Hello world"
```

Concatenation (and)

```peg
main = "Hello "  "world"
```

Referencing symbols

Symbol

```peg
main = hi
hi   = "Hello world"
```

Or conditions `/`

```peg
main = "hello" / "hi"
```

Or multiline

```peg
main
    = "hello"
    / "hi"
    / "hola"
```

Or multiline 2

```peg
main = "hello"
     / "hi"
     / "hola"
```

Or disorganized

```peg
main = "hello"
     / "hi" / "hola"
```

Parenthesis

```peg
main = ("hello" / "hi")  " world"
```

Just multiline

Multiline1

```peg
main
    = ("hello" / "hi")  " world"
```

Multiline2

```peg
main
    = ("hello" / "hi")
    " world"
```

Multiline3

```peg
main = ("hello" / "hi")
     " world"
```

It is recomended to use or operator `/` on each new line and `=` on first line, like

Multiline organized

```peg
main = ("hello" / "hi")  " world"
     / "bye"
```

One optional

```peg
main = ("hello" / "hi")  " world"?
```

Repetitions

```peg
main         = one_or_more_a / zero_or_many_b
one_or_more  = "a"+
zero_or_many = "b"*
```

Negation will not move current possition

Next example will consume all chars till get an "a"

Negation

```peg
main = (!"a" .)* "a"
```

Consume till

```peg
comment = "//" (!"\n" .)*
        / "/*" (!"*/" .)* "*/"
```

Match a set of chars.
Chars can be defined by range.

```peg
number  = digit+ ("." digit+)?
digit   = [0-9]
a_or_b  = [ab]
id      = [_a-zA-Z][_a-zA-Z0-9]*

a_or_b_or_digit  = [ab0-9]
```

Simple recursion

one or more "a" recursive

```peg
as  = "a" as
    / "a"

//  simplified with `+`
ak = "a"+
```

Recursion to match parentheses

Recursion match par

```peg
match_par = "(" match_par ")"
          / "(" ")"
```

In order to produce custom errors, you have to use `error(...)` constructor

In next example, the system will complain with parenthesis error if they are unbalanced
```peg
    parenth         =   '('  _  expr  _  (  ')'
                                         /  error("unbalanced parethesis: missing ')'") 
                                         )
```

As you can see, if you can run the rule to close properly the parenthesis, everything is OK, in other case, custom error message will be produced

### Replacing

You can set the replace rules with `->`

```text
        op      =   '+'     -> ADD
                /   '-'     -> SUB
```

When `+` will be found and validated, it will be replaced by `ADD`

```text
        expr    =   num:num                -> PUSH $(num)$(:endl)
                    (op:op  expr:expr)?    -> $(expr)EXEC $(op)$(:endl)
```

To refer to parsed chunk, you can name it using `:`

When refering to a `symbol`, you don't need to give a name

Next examples, are equivalent

```text
        expr    =   num:num                -> PUSH $(num)$(:endl)
                    (op:op  expr:expr)?    -> $(expr)EXEC $(op)$(:endl)
```

```text
        expr    =   num            -> PUSH $(num)$(:endl)
                    (op  expr)?    -> $(expr)EXEC $(op)$(:endl)
```

After the arrow, you will have the transformation rule.

`Replacing tokens`:
Things inside `$(...)` will be replaced.
Text outside it, will be written as it

`Replacing tokens` can refer to parsed text by name or by position.

```text
           -> $(num)
```

This will look for a name called `num`defined on left side to write it on output

```text
           -> $(.1)
```

You can also refer to `functions` starting the `replacing token` with `:`

```text
        expr    =   num            -> $(:endl)
```

Predefined functions are...

(Watch on `replace.rs` to see full replace functions)
```rust
        "endl" => "\n",
        "spc" => " ",
        "_" => " ",
        "tab" => "\t",
        "(" => "\t",
        // "now" => "pending",
        _ => "?unknown_fn?",
```


Example

```text
        expr    =   num            -> PUSH $(num)$(:endl)
                    (op  expr)?    -> $(.2)EXEC $(.1)$(:endl)
```

## Full math expresion compiler example

What is a parser without an math expresion calculator?

Obiously, it's necessary to consider the operator priority, operator asociativity and parenthesis, and negative numbers and negative expresions

```rust
extern crate dpr;

fn main() -> Result<(), dpr::Error> {
    let result = dpr::Peg::new(
        r#"
        main    =   expr

        expr    =   term    (
                            _  add_op   _  term     ->$(term)$(add_op)
                            )*                              

        term    =   factor  (
                            _  mult_op  _  factor   ->$(factor)$(mult_op)
                            )*                              

        factor  =   pow     (
                            _  pow_op   _  subexpr  ->$(subexpr)$(pow_op)
                            )*       

        pow     =   subexpr (
                            _  pow_op   _  pow  ->$(pow)$(pow_op)
                            )*       

        subexpr =   '(' _ expr _ ')'              ->$(expr)
                /   number                        ->PUSH $(number)$(:endl)
                /   '-' _ subexpr                 ->PUSH 0$(:endl)$(subexpr)SUB$(:endl)
                /   '(' _ expr _      error("parenthesis unbalanced")
                /       _ expr _ ')'  error("parenthesis unbalanced")

        number  =   ([0-9]+  ('.' [0-9])?)    

        add_op  =   '+'     ->EXEC ADD$(:endl)
                /   '-'     ->EXEC SUB$(:endl)

        mult_op =   '*'     ->EXEC MULT$(:endl)
                /   '/'     ->EXEC DIV$(:endl)

        pow_op  =   '^'     ->EXEC POW$(:endl)

        _       = ' '*
        "#,
    )
    .gen_rules()?
    .parse("-(-1+2* 3^5 ^(- 2 ) -7)+8")?
    .replace()?
    //  ...
    ;

    println!("{:#?}", result);
    println!("{}", result.str());
    Ok(())
}
```

The output is a program for a stack machine, composed of a command with a parameter...

```text
PUSH 0
PUSH 0
PUSH 1
EXEC SUB
PUSH 2
PUSH 3
PUSH 5
PUSH 0
PUSH 2
EXEC SUB
EXEC POW
EXEC POW
EXEC MULT
EXEC ADD
PUSH 7
EXEC SUB
EXEC SUB
PUSH 8
EXEC ADD
```