mod map;
mod matrix;

fn main() {
    let m = map::new_zigzag(5);
    println!("matrix {:#?}", m.matrix);

    for r in 0..m.matrix.size {
        for c in 0..m.matrix.size {
            print!("{} ", m.matrix.get_rc(r, c));
        }
        println!("");
    }
}
