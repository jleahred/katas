use std::thread::Thread;

fn main() {
    println!("Div zero test");

    let result = Thread::scoped(move || {
        make_a_division()
    }).join();
    //assert!(result.is_err());
    //result.is_ok

    match result {
        Err(_) => println!("FAILLED!"),
        Ok(value)=> println!("Division result...  {}", value),
    };

}
 

fn make_a_division() -> u32 {
    make_a_division1()
}


fn make_a_division1() -> u32 {
    make_a_division2()
}

fn make_a_division2 () -> u32  {
    println!("Enter divisor...");
    let line = std::io::stdin().read_line().ok().expect("error reading line");
    let divisor_option :Option<u32> = line.trim().parse();

    let divisor = match divisor_option {
        Some(divisor) => divisor,
        None          => { println!("I don't understand, I will interpret 1"); 1}
    };


    println!("readed {}", divisor);

    let dangerous =  1_000_000 / divisor;

    dangerous
}
