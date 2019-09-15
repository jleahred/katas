use peg::parser;

parser! {
    grammar math_parser() for str {
        pub rule expr() -> f64
            = number()
        // pub rule expr() -> f64
        //     = _() r:(
        //           "+" _() "("  _()  e:expr()  _() ")"  _()  { e }
        //         / "-" _() "("  _()  e:expr()  _() ")"  _()  { -1.0 * e }
        //         /         "("  _()  e:expr()  _() ")"  _()  { e }

        //         / "+"     t:term()                { t }
        //         / "-"     t:term()                { -1.0 * t }
        //         /           term()
        //         ) { r }


        // rule no_sign_expr() -> f64
        //     = _() r:(
        //           "("  _()  e:expr()  _() ")"  _()  { e }
        //         /             term()
        //         ) { r }

        // rule term() -> f64
        //     =   spc()
        //         r:( l:factor() spc() "+" spc() r:term()  { l + r }
        //         /   l:factor() spc() "-" spc() r:term()  { l - r }
        //         /     factor()
        //         )   { r }

        // rule factor() -> f64
        //     =   spc()
        //         r:( l:number() spc() "*" spc() r:factor()  { l * r }
        //         /   l:number() spc() "/" spc() r:factor()  { l / r }
        //         /     number()
        //         )   { r }


        //  number      ------------------------
        rule number() -> f64
            = n:$(['0'..='9']+("." ['0'..='9']+)?)
                {? n.parse().or_else(|_| Err("failed parsing number")) }

        //  spaces      ------------------------
        rule _()  = quiet!{[' ' | '\t' | '\n' | '\r']*}
        rule spc()  = quiet!{" "*}
    }
}

pub fn main() {}

#[test]
pub fn test_1() {
    assert_eq!(math_parser::expr("1"), Ok(1.0));
    // assert_eq!(math_parser::expr("123"), Ok(123.0));
    // assert_eq!(math_parser::expr("123.12"), Ok(123.12));
    // assert_eq!(math_parser::expr("+123.12"), Ok(123.12));
    // assert_eq!(math_parser::expr("-123.12"), Ok(-123.12));
    // assert_eq!(math_parser::expr("- 123.12"), Ok(-123.12));
    // assert_eq!(math_parser::expr("1+2"), Ok(3.0));
    // assert_eq!(math_parser::expr("1-2"), Ok(-1.0));
    // assert_eq!(math_parser::expr("+ 1 - 2"), Ok(-1.0));
    // assert_eq!(math_parser::expr("(1)"), Ok(1.0));
    // assert_eq!(math_parser::expr("((1))"), Ok(1.0));
    // assert_eq!(math_parser::expr("+1+2+3"), Ok(6.0));
    // assert_eq!(math_parser::expr("+1-2+3"), Ok(2.0));
    // assert_eq!(math_parser::expr(" (  1 + (2 + 3) ) "), Ok(6.0));
}

#[test]
pub fn test_2() {
    // assert_eq!(math_parser::expr("+(1)"), Ok(1.0));
    // assert_eq!(math_parser::expr("+(+1)"), Ok(1.0));
    // assert!(math_parser::expr("++(1)").is_err());
    // assert!(math_parser::expr(" + + (1)").is_err());
    // assert!(math_parser::expr("(++1)").is_err());
    // assert!(math_parser::expr("+1 + +2").is_err());

    // assert_eq!(math_parser::expr("3*2"), Ok(6.0));
    // assert_eq!(math_parser::expr("8/2"), Ok(4.0));
    // assert_eq!(math_parser::expr("+ 8 / 2"), Ok(4.0));
    // assert_eq!(math_parser::expr("- 8 / 2"), Ok(-4.0));

    // assert!(math_parser::expr("- +8 / 2").is_err());
    // assert!(math_parser::expr("- 8 / -2").is_err());

    // assert_eq!(math_parser::expr("1+2*3"), Ok(7.0));
    // assert_eq!(math_parser::expr("1+(2*3)"), Ok(7.0));
    // assert_eq!(math_parser::expr("(1+2)*3"), Ok(9.0));
}
