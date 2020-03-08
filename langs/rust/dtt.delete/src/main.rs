extern crate dtt;
extern crate dynparser;

use dtt::grammar;

fn main() {
    let peg_grammar = r#"

    main    =   a   /   b

    a       =  'a'  /  'A'      ->  A
    b       =  'b'  /  'B'      -> 'B'

    "#;

    // let input = "  1 + 2 * 3  ";

    let compiled_grammar = grammar::compile(peg_grammar).unwrap();
    // let output = compiled_grammar.process(input);

    // println!("result {}", output);
}

// extern crate dtt;
// extern crate dynparser;

// use dtt::grammar;

// fn main() {
//     let peg_grammar = r#"

//     main            =   _  expr  _

//     expr            =   add_t       (_  add_op  _   add_t)*
//                     /   portion_expr

//     add_t           =   fact_t      (_  fact_op _   fact_t)*

//     fact_t          =   portion_expr

//     portion_expr    =   '('  expr ')'           -> $2
//                     =   '('  expr   error("unbalanced parenthesis")
//                     /   item

//     item            =   num                     -> "PUSH $1"

//     num             =   [0-9]+ ('.' [0-9]+)?
//     add_op          =   '+'  /  '-'             -> "OPER $1"
//     fact_op         =   '*'  /  '/'             -> "OPER $1"

//     _               =   ' '*                    ->  $none

//     "#;

//     let input = "  1 + 2 * 3  ";

//     let compiled_grammar = grammar::compile(peg_grammar);
//     let output = compiled_grammar.process(input);

//     println!("result {}", output);
// }
