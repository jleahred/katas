fn main() {
    sol1();
    sol2();
    sol3();
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



fn sol3() {
    println!("{}", comp_hm(&[3, 1, 2], &[1, 4, 9]));
    println!("{}", comp_hm(&[3, 1, 2, 1, 2], &[1, 4, 9, 4, 1]));
    println!("{}", comp_hm(&[3, 1, 2], &[1, 9]));
    println!("{}", comp_hm(&[1, 1, 1, 1], &[4, 0, 0, 0]));
}




use std::collections::HashMap;

fn comp_hm(a: &[i32], b: &[i32]) -> bool {
    let mut ha = HashMap::new();
    let mut hb = HashMap::new();

    let mut sa = vec![];
    for e in a {
        sa.push(e * e);
    }

    let fill_hm = |c, hm: &mut HashMap<i32, i32>| {
        for &e in c {
            let count = hm.entry(e).or_insert(0);
            *count += 1;
        }
    };

    fill_hm(b, &mut hb);
    fill_hm(&sa, &mut ha);

    ha == hb
}
