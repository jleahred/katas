extern crate rand;

const N_PRISIONERS: u32 = 100;
const N_BOXES: u32 = N_PRISIONERS;
const N_SIMULATIONS: u32 = 100_000;

struct Boxes(Vec<u32>);

use rand::seq::SliceRandom;
use rand::thread_rng;

fn main() {
    // let boxes = gen_boxes();
    // println!("{:?}", boxes);
    // println!("{:?}", run_prisioner(1, &gen_boxes()));

    // println!("{:?}", full_simulation());

    let mut n_ok = 0;
    for _ in 0..N_SIMULATIONS {
        if full_simulation() {
            n_ok += 1;
        }
    }
    println!("{}% all saved", n_ok as f32 / N_SIMULATIONS as f32 * 100.0);
}

fn gen_boxes() -> Boxes {
    let mut boxes = (0..N_BOXES).collect::<Vec<u32>>();
    boxes.shuffle(&mut thread_rng());
    Boxes(boxes)
}

fn run_prisioner(prisioner_id: u32, boxes: &Boxes) -> u32 {
    let mut next_box = prisioner_id;
    for _ in 0..(N_BOXES / 2) {
        next_box = boxes.0[next_box as usize];
        // dbg!(next_box);
        if next_box == prisioner_id {
            return next_box;
        }
    }
    next_box
}

fn full_simulation() -> bool {
    let boxes = &gen_boxes();

    for prissioner in 0..N_PRISIONERS {
        // dbg!(prissioner);
        let result_prissioner = run_prisioner(prissioner, &boxes);
        if result_prissioner != prissioner {
            return false;
        }
    }

    true
}
