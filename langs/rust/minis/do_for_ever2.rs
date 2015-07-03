use std::thread::Thread;

fn main() {
    println!("Make divisions for ever");
    println!("Humble let it crash");

    //  and ever
    loop {
        let result = Thread::scoped(move || {
            make_a_division_for_ever()
        }).join();
        if result.is_ok() {
            break;  //  it will never hapend in this case
        }
        else {
            println!("It CRASHED!!!  restarting...");
        }     
    }

}

fn make_a_division_for_ever() {
    loop {
        println!("Enter divisor...");
        //  reducing boilerplate with let it crash (unwrap)        
        let line = std::io::stdin().read_line().ok().expect("error reading line");

        //  reducing boilerplate with let it crash (unwrap)        
        let divisor = line.trim().parse::<u32>().expect("
I coudn't parse your line as an string. I'm going to die
I showed things closer than orion belt...
        ");
        // it is also possible...  let divisor:u32 = line.trim().parse().expect("

        println!("readed {}", divisor);

        let dangerous =  1_000_000 / divisor;

        println!("DIV RESULT... {}", dangerous);
    }
}
 
