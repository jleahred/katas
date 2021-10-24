extern crate parkit;
use parkit::expr::builders::*;
use parkit::*;

// fn main() {
//     let rules = rules! {
//            "main" =>    rule!(ref_rule("expr")),

//             "expr" =>    rule!(
//                 or!(
//                     and!(
//                         ref_rule("expr"),
//                         ref_rule("op"),
//                         ref_rule("expr")
//                     ),
//                     and!(lit("("), ref_rule("expr"), lit(")")),
//                     and!(ref_rule("atom"))
//                 )
//             ),
//             "op" =>    rule!(
//                 or!(
//                         lit("+"),
//                         lit("-"),
//                         lit("*"),
//                         lit("/")
//                 )
//             ),
//             "atom" =>    rule!(
//                 or!(
//                         lit("0"),
//                         lit("1")
//                 )
//             )
//     };

//     let p = parse("1+1", &rules);
//     print!("{:?}", p);
// }

fn main() {
    let rules = rules! {
           "main"   =>    rule!(ref_rule("as")),

           "as"     =>    rule!(
                or!(
                    and!(
                        lit("a"),
                        ref_rule("as")
                    ),
                    lit(".")
                )
            )
    };

    let r = parse("aaaaaáa.....", &rules);
    match r {
        Ok(()) => print!("OK"),
        Err(e) => print!("{}", e),
    }
}
