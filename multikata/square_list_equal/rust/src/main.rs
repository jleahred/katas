fn main() {
    sol1();
    sol2();
}



macro_rules! comp {
    ($a:expr, $b:expr) => {
        {
            let mut nwa = $a.clone();
            let mut nwb = $b.clone();
            for mut e in &mut nwa {
                *e = *e * *e;
            };
            nwa.sort();
            nwb.sort();
            nwa == nwb
        }
    };
}

fn sol1() {
    let a = [3, 1, 2];
    let b = [1, 4, 9];

    println!("{}", comp!(a, b));

    let va = vec![3, 1, 2];
    let vb = vec![1, 4, 9];

    println!("{}", comp!(va, vb));
}




fn sol2() {
    let a = vec![3, 1, 2];
    let b = vec![1, 4, 9];

    println!("{}", comp(a, b));
}


fn comp(mut a: Vec<i32>, mut b: Vec<i32>) -> bool {
    for mut e in &mut a {
        *e = *e * *e;
    }
    a.sort();
    b.sort();
    a == b
}
