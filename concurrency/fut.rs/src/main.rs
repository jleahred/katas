extern crate futures;
extern crate futures_cpupool;

use futures::Future;
use futures_cpupool::CpuPool;
use std::io::Write;


const BIG_PRIME: u64 = 15485867;

// checks whether a number is prime, slowly
fn is_prime(num: u64) -> bool {
    for i in 2..num {
        for _ in 0..5 {
            if num % i == 0 {
                return false;
            }
        }
    }
    true
}


fn main() {
    // set up a thread pool
    // let pool = CpuPool::new_num_cpus();
    let pool = CpuPool::new(1);

    // spawn our computation, getting back a *future* of the answer
    let prime_future = pool.spawn_fn(|| {
        let prime = is_prime(BIG_PRIME);

        // For reasons we'll see later, we need to return a Result here
        let res: Result<bool, ()> = Ok(prime);
        print!("Prime solved");
        res
    });

    let printdots_future = pool.spawn_fn(|| {
        for _ in 0..1000 {
            print!(".");
            std::io::stdout().flush();
        }
        let res: Result<(), ()> = Ok(());
        res
    });


    println!("Created the future");

    let _ = printdots_future.wait();
    // unwrap here since we know the result is Ok
    if prime_future.wait().unwrap() {
        println!("Prime");
    } else {
        println!("Not prime");
    }
}