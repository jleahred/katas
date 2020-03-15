# DTT

- [repository](https://github.com/jleahred/dtt)
- [doc](https://docs.rs/dtt/)
- [rust-crate](https://crates.io/crates/dtt)

Using [dynparser](https://github.com/jleahred/dynparser)


## Simple example

Peg:
```text
    main    =   a+
    a       =   'a'     ->b
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


   .--------.          .--------.
   |  peg   |          | input  |
   |  user  |          |  user  |
   '--------'          '--------'
        |                   |
        |                   |
        v                   |
   .---------.              |
   |   peg   |              |
   | exp-set |              v
   '---------'         .---------.
        |              | process |
        '------------->|  input  |
                       '---------'
                            |
                            |
                            v
                       .--------.
                       |  AST   |
                       | input  |
                       '--------'
                            |
                            |
                            v
                       .--------.
                       |  run   |
                       |  AST   |<--------make transformations
                       '--------'
                            |
                            |
                            v
                       .--------.
                       | OUTPUT |
                       |        |
                       '--------'

```