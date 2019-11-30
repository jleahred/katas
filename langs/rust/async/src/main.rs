// use async_std;
// use async_std::task;
// use tokio::runtime::Builder;

// #[tokio::main]
// async fn main() {
//     // build runtime
//     let rt = Builder::new()
//         .threaded_scheduler()
//         .num_threads(1)
//         // .thread_name("my-custom-name")
//         // .thread_stack_size(3 * 1024 * 1024)
//         .build()
//         .unwrap();
//     let handle = rt.spawn(async {
//         let mut count = 0;
//         loop {
//             count += 1;
//             print!("########({})", count);
//             task::yield_now().await;
//         }

//         // Return a value for the example
//         // "result of the computation"
//     });

//     let _handle2 = rt.spawn(async {
//         let mut count = 0;
//         loop {
//             count += 1;
//             print!("________({})", count);
//             task::yield_now().await;
//         }

//         // Return a value for the example
//         // "result of the computation"
//     });

//     // Wait for the spawned task to finish
//     let res = handle.await;
//     // let _res2 = handle2.await;

//     println!("got {:?}", res);
// }

// fn main() {
//     async_std::task::block_on(async {
//         let mut count = 0;
//         loop {
//             count += 1;
//             print!("########({})", count);
//             // task::yield_now().await;
//         }
//     });
// }

// fn main() {
//     let mut rt = tokio::runtime::Runtime::new().unwrap();
//     rt.block_on(async {
//         let mut count = 0;
//         loop {
//             count += 1;
//             print!("########({})", count);
//             // task::yield_now().await;
//         }
//     });
// }

// fn main() {
//     let mut rt = tokio::runtime::Runtime::new().unwrap();
//     let handle = rt.spawn(async {
//         let mut count = 0;
//         loop {
//             count += 1;
//             print!("########({})", count);
//             task::yield_now().await;
//         }
//     });
//     let _ = rt.block_on(handle);
// }

// fn main() {
//     let mut rt = tokio::runtime::Runtime::new().unwrap();
//     let handle = rt.spawn(async {
//         let mut count = 0;
//         loop {
//             count += 1;
//             print!("########({})", count);
//             task::yield_now().await;
//         }
//     });
//     let _ = rt.spawn(async {
//         let mut count = 0;
//         loop {
//             count += 1;
//             print!("________({})", count);
//             task::yield_now().await;
//         }
//     });
//     let _ = rt.block_on(handle);
// }

// fn main() {
//     let mut rt = tokio::runtime::Runtime::new().unwrap();
//     let handle = rt.spawn(async {
//         for count in 1..10 {
//             print!("########({})", count);
//             // task::yield_now().await; //  with and without O_O
//         }
//         "finished 1"
//     });
//     let _ = rt.spawn(async {
//         for count in 1..100 {
//             print!("________({})", count);
//             // task::yield_now().await; //  with and without O_O
//         }
//         "finished 2"
//     });
//     let r = rt.block_on(handle);
//     println!("{:?}", r)
// }

use std::thread;

fn main() {
    // let th = thread::spawn(move || loop {
    //     print!("a")
    // });
    // let _ = thread::spawn(move || loop {
    //     print!("b")
    // });
    // let _ = th.join();

    let th = thread::spawn(|| 42);
    println!("{:?}", th.join())
}
