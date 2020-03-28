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

- making equivalent

```txt
    main    =   'b' as:as
    as      =   'a'+
```

```txt
    main    =   'b' as
    as      =   'a'+
```

## Simple example

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
    expr    =   num:num  op:op  expr:expr   
                    ->PUSH $(num)
                    ->$(expr)
                    ->EXEC $op
                      

            /   num ->PUSH $(num)$(c __endl__)


    op      =   '+'     ->ADD
            /   '-'     ->SUB
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

Now you can see

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