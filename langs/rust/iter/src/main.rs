fn main() {
    let t: Vec<_> = triangulars()
        .map(|n| (n, num_factors(n)))
        .filter(|&(_, nf)| nf > 100)
        .take(1)
        .collect();

    println!("{:?}", t);
}





#[derive(Debug)]
struct Triangular {
    n: u64,
    val: u64,
}


fn triangulars() -> Triangular {
    Triangular { n: 0, val: 0 }
}

impl Iterator for Triangular {
    type Item = u64;

    fn next(&mut self) -> Option<u64> {
        self.n += 1;
        self.val += self.n;
        Some(self.val)
    }
}




fn num_factors(n: u64) -> u32 {
    let mut result = 1;
    for i in 2..n - 1 {
        use std::cmp::Ordering::{Less, Equal, Greater};
        match (i * i).cmp(&n) {
            Equal => {
                result = result * 2 + 1;
                break;
            }
            Greater => {
                result = result * 2;
                break;
            }
            Less => (),
        }
        if n % i == 0 {
            result += 1;
        }
    }

    result
}