extern crate parkit;
use parkit::expr::builders::*;
use parkit::*;

fn main() {
    let rules = rules! {
           "main" =>    rule!(ref_rule("expr")),

            "expr" =>    rule!(
                or!(
                    and!(
                        ref_rule("expr"),
                        ref_rule("op"),
                        ref_rule("expr")
                    ),
                    and!(lit("("), ref_rule("expr"), lit(")")),
                    and!(ref_rule("atom"))
                )
            ),
            "op" =>    rule!(
                or!(
                        lit("+"),
                        lit("-"),
                        lit("*"),
                        lit("/")
                )
            ),
            "atom" =>    rule!(
                or!(
                        lit("0"),
                        lit("1")
                )
            )
    };

    // let r = parse("aaaaa", &rules).ok().unwrap();
    // print!("{:?}", r);

    // let p = parse("a", &rules);
    // print!("{:?}", p);
    // let p = parse("aaaa", &rules);
    // print!("{:?}", p);
    // let p = parse("aaaaaaa", &rules);
    // print!("{:?}", p);
    // let p = parse("aaaaaaaaaaaaaaaa", &rules);
    // print!("{:?}", p);
    let p = parse("1+1", &rules);
    print!("{:?}", p);
}
