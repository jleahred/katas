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
        assert!(expr("+1*8") == Ok(8.0));
        assert!(expr("-2.0*0.5") == Ok(-1.0));
        assert!(expr("5*2") == Ok(10.0));
        assert!(expr("1+2*3") == Ok(7.0));
        println!("{:?}", expr("4*2*3-5"));
        assert!(expr("4*2*3-5") == Ok(19.0));
        assert!(expr("-4*2-3*5") == Ok(-23.0));
    }

}


fn main() {
    println!("Hello, world!");
}
