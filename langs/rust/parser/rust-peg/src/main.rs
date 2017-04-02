mod my_grammar {
    include!(concat!(env!("OUT_DIR"), "/test_grammar.rs"));
}


fn main() {
    println!("Hello, world!");
    match my_grammar::expression("+1.0") {
        Ok(r) => println!("Parsed as: {:?}", r),
        Err(e) => println!("Parse error: {}", e),
    }
    match my_grammar::expression("-1.0") {
        Ok(r) => println!("Parsed as: {:?}", r),
        Err(e) => println!("Parse error: {}", e),
    }
}
