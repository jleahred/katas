extern crate eval;

fn main() {
    let txt_expr = "1+2*3";
    let compiled = eval::expr::compile(txt_expr);
    println!("{:?}", compiled);
    println!("{:?}", eval::run(compiled.unwrap()));
}
