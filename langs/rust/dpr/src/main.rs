// extern crate dpr;

// use dpr::peg::peg2code;
// fn main() {
//     peg2code::print_rules2parse_peg();
// }

extern crate dpr;

fn main() -> Result<(), dpr::Error> {
    let result = dpr::Peg::new(
        r#"
        main    =   expr

        expr    =   term    (
                            _  add_op   _  term     ->$(term)$(add_op)
                            )*                              

        term    =   factor  (
                            _  mult_op  _  factor   ->$(factor)$(mult_op)
                            )*                              

        factor  =   pow     (
                            _  pow_op   _  subexpr  ->$(subexpr)$(pow_op)
                            )*       

        pow     =   subexpr (
                            _  pow_op   _  pow  ->$(pow)$(pow_op)
                            )*       

        subexpr =   '(' _ expr _ ')'              ->$(expr)
                /   number                        ->PUSH $(number)$(:endl)
                /   '-' _ subexpr                 ->PUSH 0$(:endl)$(subexpr)EXEC SUB$(:endl)
                /   '(' _ expr _      error("parenthesis unbalanced")
                /       _ expr _ ')'  error("parenthesis unbalanced")

        number  =   ([0-9]+  ('.' [0-9])?)    

        add_op  =   '+'     ->EXEC ADD$(:endl)
                /   '-'     ->EXEC SUB$(:endl)

        mult_op =   '*'     ->EXEC MULT$(:endl)
                /   '/'     ->EXEC DIV$(:endl)

        pow_op  =   '^'     ->EXEC POW$(:endl)

        _       = ' '*
        "#,
    )
    .gen_rules()?
    .parse("-(-1+2* 3^5 ^(- 2 ) -7)+8")?
    // .parse("-(1))")?
    .replace()?
    //  ...
    ;

    println!("{:#?}", result);
    println!("{}", result.str());
    Ok(())
}
