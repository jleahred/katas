# Calculator

This is a small example of an arithmetic calculator written in RUST using [rust-peg](https://github.com/kevinmehall/rust-peg)

It's fully interpreted

The grammar and interpreter code is on [rust-peg](https://github.com/kevinmehall/rust-peg) format


Examples
```rust
    println!("{:?}", calc::expr("1+2*3"));
    println!("{:?}", calc::expr("2*(1+pow(2,3))"));
```




PEG Grammar and interpreter
[here](./src/calculator.rustpeg)



Rust test and example
[here](./src/main.rs)
