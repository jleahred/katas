fn main() {
    println!("Hello, world!");

    // let fib = Fibonacci::new(4895);

    // println!("{:?}", fib.last());
}

// fn product_fib(prod: u64) -> (u64, u64, bool) {
//     let (mut a, mut b) = (0u64, 1u64);
//     while a * b < prod {
//         let c = a + b;
//         a = b;
//         b = c;
//     }
//     (a, b, a * b == prod)
// }

struct FibIter(u64, u64);

impl Iterator for FibIter {
    type Item = Self;
    fn next(&mut self) -> Option<Self> {
        Some(std::mem::replace(self, FibIter(self.1, self.0 + self.1)))
    }
}

fn product_fib(prod: u64) -> (u64, u64, bool) {
    FibIter(0, 1)
    .find(|FibIter(a, b)| a*b >= prod)
    .map(|FibIter(a, b)| (a, b, a*b==prod))
    .unwrap()

    // let FibIter(a, b) = FibIter(0, 1)
    //     .take_while(|FibIter(a, b)| a * b <= prod)
    //     .last()
    //     .unwrap();
    // if a*b == prod {
    //     (a,b, true)
    // } else {
    //     (b, a+b, false)
    // }
}

// extern crate itertools;
// use itertools::iterate;

// fn product_fib(prod: u64) -> (u64, u64, bool) {
//     iterate((1, 1), |&(x, y)| (y, x + y))
//         .find(|(x, y)| x * y >= prod)
//         .map(|(x, y)| (x, y, x * y == prod))
//         .unwrap()
// }

// struct Fibonacci {
//     a: u64,
//     b: u64,
//     limit: u64,
// }

// impl Fibonacci {
//     fn new(limit: u64) -> Fibonacci {
//         Fibonacci { a: 0, b: 1, limit }
//     }
// }

// impl Iterator for Fibonacci {
//     type Item = (u64, u64);

//     fn next(&mut self) -> Option<Self::Item> {
//         let next_fib = self.a + self.b;
//         self.a = self.b;
//         self.b = next_fib;

//         if self.a * self.b > self.limit {
//             None
//         } else {
//             Some((self.a, self.b))
//         }
//     }
// }

// fn product_fib(prod: u64) -> (u64, u64, bool) {
//     let (a, b) = Fibonacci::new(prod).last().unwrap();
//     match a * b == prod {
//         true => (a, b, true),
//         false => (b, a + b, false),
//     }
// }

fn dotest(prod: u64, exp: (u64, u64, bool)) {
    assert_eq!(product_fib(prod), exp)
}

#[test]
fn basics_product_fib() {
    dotest(4895, (55, 89, true));
    dotest(5895, (89, 144, false));
}
