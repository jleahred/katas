
extern crate dtt;

//  generate rules to parse peg ;-)
// extern crate dynparser;

// fn main() {
//     dtt::peg::peg2code::print_rules2parse_peg();
// }

extern crate dynparser;

fn main() {
    let peg = "
    main    =   a+
    a       =   name:'a'  
    ";
    let rules = dtt::peg::rules_from_peg(peg);
    println!("{:#?}", rules);
}
