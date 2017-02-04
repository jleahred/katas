use std::collections::linked_list::LinkedList;
use std::vec::Vec;
use std::string::String;

extern crate rand;
extern crate time;
use time::PreciseTime;


macro_rules! machine_list {
    ( $( { $mcode: expr, [ $( ($num: expr,  $cost: expr) ),* ] } ),* ) => {{
        let mut machines = ::std::collections::LinkedList::<Machine>::new();
        $( 
            let mut lvisit_info = Vec::<VisitInfo>::new();
            $( lvisit_info.push(VisitInfo{num: $num, cost: $cost}); )*
            let machine = Machine{code: $mcode.to_string(), visit_info: lvisit_info};
            machines.push_back(machine);
        )*
        machines
    }}
}



#[derive(Debug)]
struct VisitInfo {
    num: u16,
    cost: u16,
}


#[derive(Debug)]
struct Machine {
    code: String,
    visit_info: Vec<VisitInfo>,
}




fn main() {
    // let machines = machine_list!{
    //     //  machine_code, [ (num_visits, minut_visits), (num_visits, minut_visits), ...]
    //     { "m1", [( 6, 15), ( 2, 8), ( 4, 12)] },
    //     { "m2", [( 5, 15), ( 4, 8), ( 3, 12)] }
    // };


    // machine  A	B	C	time A	    time B	    time C
    // 1        8	2	1	15	        20	        30
    // 2	    7	3	1	20	        30	        50
    // 3	    6	4	1	25	        35	        55
    // 4	    8	2	1	15	        20	        30
    // 5	    8	2	1	12	        15	        28
    // 6	    8	2	1	15	        20	        30
    // 7	    7	3	1	20	        30	        50
    // 8	    6	4	1	25	        35	        55
    // 9	    8	2	1	15	        20	        30
    // 10	    8	2	1	12	        15	        28
    // 11	    8  	2	1	15	        20	        30
    // 12	    8	2	1	12	        15	        28

    let machines = machine_list!{
        //  machine_code, [ (num_visits, minut_visits), (num_visits, minut_visits), ...]
        { "m1",  [( 8, 15), ( 2, 20), ( 2, 30)] },
        { "m2",  [( 7, 20), ( 3, 30), ( 2, 50)] },
        { "m3",  [( 6, 25), ( 4, 35), ( 2, 55)] },
        { "m4",  [( 8, 15), ( 2, 20), ( 2, 30)] },
        { "m5",  [( 8, 12), ( 2, 15), ( 2, 28)] },
        { "m6",  [( 8, 15), ( 2, 20), ( 2, 30)] },
        { "m7",  [( 7, 20), ( 3, 30), ( 2, 50)] },
        { "m8",  [( 6, 25), ( 4, 35), ( 2, 55)] },
        { "m9",  [( 8, 15), ( 2, 20), ( 2, 30)] },
        { "m10", [( 8, 12), ( 2, 15), ( 2, 28)] },
        { "m11", [( 8, 15), ( 2, 20), ( 2, 30)] },
        { "m11", [( 8, 12), ( 2, 15), ( 2, 28)] }
    };



    let n_visits = get_machine_visits(&machines);

    println!("Number of groups: {:?}", n_visits);


    find_best_year(&machines);
}


fn find_best_year(machines: &LinkedList<Machine>) -> () {
    let mut best_year;
    let mut best_value = u16::max_value();
    let mut chrono = PreciseTime::now();
    let write_each = 1_000_000;

    for i in 1.. {
        if i % write_each == 0 {
            let new_chrono = PreciseTime::now();
            let secs = chrono.to(new_chrono).num_microseconds().unwrap() as f32 / 1_000_000.0;
            println!("\rtranstacts/sec {:?}  total transc: {:?}",
                     (write_each as f32) / secs,
                     (i as f32) / (write_each as f32));
            chrono = new_chrono;
        }
        let new_year = get_random_year(machines);
        let new_value = calculate_value_year(&new_year);
        if new_value < best_value {
            best_year = new_year;
            best_value = new_value;
            print_better(&best_value, &best_year);
        }
    }
}

fn print_better(best_value: &u16, best_year: &Vec<(String, LinkedList<(u16, u16)>)>) {
    println!("BETTER: {:?}", best_value);
    for &(ref machine, ref year) in best_year {
        println!("{:?} {:?}", machine, year);
    }
    println!("");
}









fn get_machine_visits(ml: &LinkedList<Machine>) -> u16 {
    let n_groups = ml.front().unwrap().sum_visits();

    for machine in ml.iter() {
        if n_groups != machine.sum_visits() {
            panic!("ERROR...   Invalid sum visits for machine: {:?}", machine);
        }
    }
    n_groups
}

impl Machine {
    fn sum_visits(&self) -> u16 {
        self.visit_info.iter().fold(0, |acc, vi| acc + vi.num)
    }

    fn get_random_visits(&self) -> LinkedList<(u16, u16)> {
        let mut vis_rand = Vec::new();
        for (i, vi) in self.visit_info.iter().enumerate() {
            for _ in 0..vi.num {
                vis_rand.push((i as u16, vi.cost, rand::random::<u16>()));
            }
        }
        vis_rand.sort_by(|&(_, _, ar), &(_, _, br)| ar.cmp(&br));
        vis_rand.iter().map(|&(i, cost, _)| (i, cost)).collect()
    }
}



fn get_random_year(machines: &LinkedList<Machine>) -> Vec<(String, LinkedList<(u16, u16)>)> {
    machines.iter().map(|m| (m.code.clone(), m.get_random_visits())).collect::<Vec<_>>()
}

fn cost_per_interval(year: &Vec<(String, LinkedList<(u16, u16)>)>) -> Vec<u16> {
    let mut result = vec![0; 12];

    for &(ref _mcode, ref myear) in year {
        for (i, &(_idx, cost)) in myear.iter().enumerate() {
            result[i] += cost;
        }
    }
    result
}

fn calculate_value_month_cost(mc: &Vec<u16>) -> u16 {
    let (mut min, mut max) = (u16::max_value(), 0);
    for item in mc {
        if *item < min {
            min = *item
        }
        if *item > max {
            max = *item
        }
    }
    max - min
}

fn calculate_value_year(year: &Vec<(String, LinkedList<(u16, u16)>)>) -> u16 {
    calculate_value_month_cost(&cost_per_interval(year))
}