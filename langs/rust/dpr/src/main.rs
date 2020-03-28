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

        expr    =   num:num                -> PUSH $(num)$(:endl)
                    (op:op  expr:expr)?    -> $(expr)EXEC $(op)$(:endl)

        op      =   '+'     -> ADD
                /   '-'     -> SUB

        num     =   [0-9]+  ('.' [0-9])?
        ",
    )
    .gen_rules()?
    .parse("1+2")?
    .replace()?
    //  ...
    ;

    println!("{:#?}", result);
    Ok(())
}
