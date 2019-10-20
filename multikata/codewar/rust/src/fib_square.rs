fn perimeter(n: u64) -> u64 {
    FibIter(0, 1).take(n as usize + 1).sum::<u64>() * 4
}

struct FibIter(u64, u64);

// impl FibIter {
//     fn new() -> FibIter {
//         FibIter(0, 1)
//     }
// }

impl Iterator for FibIter {
    type Item = u64;
    fn next(&mut self) -> Option<Self::Item> {
        std::mem::swap(&mut self.0, &mut self.1);
        self.1 += self.0;
        Some(self.0)
    }
}

fn dotest(n: u64, exp: u64) -> () {
    assert_eq!(perimeter(n), exp)
}

// #[test]
// fn fib_test() {
//     println!("_____ {:?}", FibIter::new().take(5).collect::<Vec<u64>>());
//     assert!(FibIter(1, 1).take(2).sum::<u64>() == 2)
// }

#[test]
fn basics_perimeter() {
    dotest(5, 80);
    dotest(7, 216);
    dotest(20, 114624);
    dotest(30, 14098308);
}
