// extern crate dpr;
// use dpr::peg::peg2code;

// fn main() {
//     peg2code::print_rules2parse_peg();
// }

extern crate dpr;

fn main() -> Result<(), dpr::Error> {
    let result = dpr::Peg::new(
        "
        main    =   'b'  as:a+
        a       =   'a' -> r
                    'b' 'c' -> s
    ",
    )
    .gen_rules()?
    .parse("babc")?
    .replace()?
    //  ...
    ;

    println!("{:#?}", result);
    Ok(())
}
