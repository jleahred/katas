// extern crate dpr;
// use dpr::peg::peg2code;

// fn main() {
//     peg2code::print_rules2parse_peg();
// }

// extern crate dpr;
// fn main() {
//     let peg = "
//     main    =   as:a+
//     a       =   a:'a'
//     ";
//     let rules = dpr::peg::rules_from_peg(peg);
//     println!("{:#?}", rules);
// }

extern crate dpr;
fn main() {
    let peg = "
        main    =   'b' as:a+
        a       =   a:'a'
    ";
    let rules = dpr::peg::rules_from_peg(peg).unwrap();
    let result = dpr::parse("baa", &rules).unwrap()/*.flatten()*/;
    println!("{:#?}", result);
}
