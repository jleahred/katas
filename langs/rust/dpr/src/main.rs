// extern crate dpr;
// use dpr::peg::peg2code;

// fn main() {
//     peg2code::print_rules2parse_peg();
// }

extern crate dpr;

fn main() -> Result<(), dpr::Error> {
    let result = dpr::Peg::new(
        "
        main    =   expr
        expr    =   num:num  op:op  expr:expr   -> PUSH $(num)$(expr)EXEC $(op)
                /   num                         -> PUSH $(num)$(:__endl__)
        op      =   '+'     ->ADD
                /   '-'     ->SUB

        num     =   [0-9]+  ('.' [0-9])?
        ",
    )
    .gen_rules()?
    .parse("1+2")?
    // .replace()?
    //  ...
    ;

    println!("{:#?}", result);
    Ok(())
}
