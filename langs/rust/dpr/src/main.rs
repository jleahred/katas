// extern crate dpr;
// use dpr::peg::peg2code;

// fn main() {
//     peg2code::print_rules2parse_peg();
// }

// extern crate dpr;
// fn main() -> Result<(), dpr::Error> {
//     let ast = dpr::Peg::new(
//         "
//         main    =   'b' as:a+
//         a       =   a:'a'       -> b
//     ",
//     )
//     .gen_rules()?
//     .parse("baa")?;

//     println!("{:#?}", ast);
//     Ok(())
// }

extern crate dpr;
use dpr::peg::rules_from_peg;
fn main() {
    let rules = rules_from_peg(
        r#"
             main    =   'hello'   ' '   'world'  dot
             dot     =   "\0x2E"
         "#,
    )
    .map_err(|e| {
        println!("{}", e);
        panic!("FAIL");
    })
    .unwrap();
    println!("{:#?}", rules);
    let result = rules.parse("hello world.");
    assert!(result.is_ok());
    match result {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => println!("Error: {:?}", e),
    };
}
