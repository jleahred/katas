mod calc {
    include!(concat!(env!("OUT_DIR"), "/calculator.rs"));

    #[test]
    fn simple_numbers() {
        assert!(expr("1.0") == Ok(1.0));
        assert!(expr("0.1") == Ok(0.1));
    }

    #[test]
    fn sign_numbers() {
        assert!(expr("+1.0") == Ok(1.0));
        assert!(expr("-0.1") == Ok(-0.1));
    }

    #[test]
    fn int_numbers() {
        assert!(expr("+1") == Ok(1.0));
        assert!(expr("-2") == Ok(-2.0));
        assert!(expr("1") == Ok(1.0));
        assert!(expr("2") == Ok(2.0));
    }

    #[test]
    fn sum() {
        assert!(expr("+1-8") == Ok(-7.0));
        assert!(expr("-2.1+0.8") == Ok(-1.3));
        assert!(expr("1+2") == Ok(3.0));
        assert!(expr("2-5") == Ok(-3.0));
        assert!(expr("1+2+3") == Ok(6.0));
        assert!(expr("-1+2+3-5") == Ok(-1.0));
    }

    #[test]
    fn prod() {
        assert!(expr("5*2") == Ok(10.0));
        assert!(expr("+1*8") == Ok(8.0));
        assert!(expr("-2.0*0.5") == Ok(-1.0));
        assert!(expr("4*2*3") == Ok(24.0));
        assert!(expr("4*2*3*10") == Ok(240.0));
        assert!(expr("1+2*3") == Ok(7.0));
        assert!(expr("4*2*3-5") == Ok(19.0));
        assert!(expr("4*2*3-5*2") == Ok(14.0));
        assert!(expr("4*2-3*5") == Ok(-7.0));
        assert!(expr("-4*2-3*5") == Ok(-23.0));
    }

    #[test]
    fn parenthesis() {
        assert!(expr("(1+2)*3") == Ok(9.0));
        assert!(expr("(5*2)") == Ok(10.0));
        assert!(expr("+1*8") == Ok(8.0));
        assert!(expr("-2.0*0.5") == Ok(-1.0));
        assert!(expr("4*(2*3)") == Ok(24.0));
        assert!(expr("((4*2)*((3))*10)") == Ok(240.0));
        assert!(expr("4*2*(3-5)") == Ok(-16.0));
        assert!(expr("(4*2)*(3-5)*2") == Ok(-32.0));
        assert!(expr("+(-4*2)-(+3*5)") == Ok(-23.0));
    }


    #[test]
    fn simple_function() {
        assert!(expr("1+pow(2,3)") == Ok(9.0));
        assert!(expr("1+pow(1+1, 2+1)") == Ok(9.0));
    }

    #[test]
    fn spaces() {
        assert!(expr(" ( 1 + 2 ) * 3   ") == Ok(9.0));
        assert!(expr("+1 *  8 ") == Ok(8.0));
        assert!(expr("  (  (  4  *  2  )  *  (  (  3  )  )  *  10  )  ") == Ok(240.0));
        assert!(expr("  (  (  4  *  2  )  *  (  (  3  )  )  *  10  )  // this is a comment") ==
                Ok(240.0));
        assert!(expr("  (  (  4  *  2 /* comment */ )  *  (  (  3  )  )  *  10  )  // this \
                      is a comment") == Ok(240.0));
        assert!(expr(" 1 + pow ( 2 , 3 ) ") == Ok(9.0));
    }

    #[test]
    fn incorrect_formats() {
        assert!(expr("1+2**3").is_err());
        assert!(expr("1++2*3").is_err());
        assert!(expr("++1+2*3").is_err());
        assert!(expr("-+1+2*3").is_err());
        assert!(expr("+-1+2*3").is_err());
        assert!(expr("(1+2)()*3").is_err());
        assert!(expr("(1+2)*(3))").is_err());
        assert!(expr("(1+2+)*(3)").is_err());
        assert!(expr("(1+2*)*(3)").is_err());
        assert!(expr("(1+2*)/").is_err());
        assert!(expr("(1+2*)+").is_err());
        assert!(expr("*1+2").is_err());
        assert!(expr("+ 1+2").is_err());
        assert!(expr("1+pow(2)").is_err());
    }

}


fn main() {
    println!("{:?}", calc::expr("1+2*3"));
    println!("{:?}", calc::expr("2*(1+pow(2,3))"));
}
