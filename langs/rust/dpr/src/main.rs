// extern crate dpr;
// use dpr::peg::peg2code;

// fn main() {
//     peg2code::print_rules2parse_peg();
// }

extern crate dpr;
fn main() -> Result<(), dpr::Error> {
    let ast = dpr::Peg::new(
        "
        main    =   'b' as:a+
        a       =   a:'a'       -> b
    ",
    )
    .gen_rules()?
    .parse("baa")?;

    println!("{:#?}", ast);
    Ok(())
}
