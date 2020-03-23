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
dpr = "0.1.0"
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

Peg:
```text
    main    =   as:a+
    a       =   a:'a'     ->b
```

Input:
```text
    aaaaaa
```

Output:
```text
    bbbbbb
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