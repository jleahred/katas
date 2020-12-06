fn main() {
    println!(
        "first 100 primes are: {:?}",
        primes::iter().take(100).collect::<Vec<_>>()
    );

    let pos_prime = 1_000_000;

    // let prime_at_pos = primes::iter().skip(pos_prime - 1).next().unwrap(); //  I know there is always one ;-)
    // let prime_at_pos = primes::iter().take(pos_prime).last().unwrap(); //  I know there is always one ;-)

    println!("{:?}", primes::iter().nth(pos_prime - 1).unwrap()); // unwrap... I know there is always one ;-)
}

mod primes {
    pub struct IterPrimes(Vec<u64>);

    pub fn iter() -> IterPrimes {
        IterPrimes(vec![])
    }

    impl Iterator for IterPrimes {
        type Item = u64;

        fn next(&mut self) -> Option<u64> {
            let next_prime = next_prime(&self.0);
            self.0.push(next_prime);
            Some(next_prime)
        }
    }

    fn next_prime(prime_list: &[u64]) -> u64 {
        match prime_list.last() {
            None => 1,
            Some(last) => (last + 1..)
                .into_iter()
                .filter(|n| is_prime(*n, &prime_list[1..]))
                .next()
                .unwrap(), //  I know there is always one ;-)
        }
    }

    fn is_prime(num: u64, primes: &[u64]) -> bool {
        prime_factors(num, primes).next().is_some() == false
    }

    fn prime_factors(num: u64, primes: &[u64]) -> impl Iterator<Item = (u64, u64)> + '_ {
        primes
            .iter()
            .take_while(move |&p| p * p <= num)
            .filter(move |&p| num % p == 0)
            .map(move |&f| (f, num / f))
    }
}
