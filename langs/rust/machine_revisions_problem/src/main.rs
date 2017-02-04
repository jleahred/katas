use std::collections::linked_list::LinkedList;
use std::vec::Vec;

extern crate rand;


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


// macro_rules! list {
//     ($( [$num: expr,  $cost: expr ]),*) => {{
//          let mut list = ::std::collections::LinkedList::new();
//          $( list.push_back(visit_info{num: $num, cost: $cost}); )*
//          list
//     }}
// }



#[derive(Debug)]
struct VisitInfo {
    num: i16,
    cost: i16,
}


#[derive(Debug)]
struct Machine {
    code: std::string::String,
    visit_info: Vec<VisitInfo>,
}




fn main() {
    let machines = machine_list!{
        //  machine_code, [ (num_visits, minut_visits), (num_visits, minut_visits), ...]
        { "m1", [( 6, 15), ( 2, 8), ( 4, 12)] },
        { "m2", [( 5, 15), ( 4, 8), ( 3, 12)] }
    };
    let n_visits = get_machine_visits(&machines);

    println!("Number of groups: {:?}", n_visits);


    find_best_year(&machines);

    // println!("\n\nRANDOM...  {:?}",
    //          machines.front().unwrap().get_random_visits());

    // for m in machines {
    //     println!("{:?}", m);
    // }
}








fn get_machine_visits(ml: &LinkedList<Machine>) -> i16 {
    let n_groups = ml.front().unwrap().sum_visits();

    for machine in ml.iter() {
        if n_groups != machine.sum_visits() {
            panic!("ERROR...   Invalid sum visits for machine: {:?}", machine);
        }
    }
    n_groups
}

impl Machine {
    fn sum_visits(&self) -> i16 {
        self.visit_info.iter().fold(0, |acc, vi| acc + vi.num)
    }

    fn get_random_visits(&self) -> LinkedList<(i16, i16)> {
        let mut vis_rand = Vec::new();
        for (i, vi) in self.visit_info.iter().enumerate() {
            for _ in 0..vi.num {
                vis_rand.push((i as i16, vi.cost, rand::random::<i16>()));
            }
        }
        vis_rand.sort_by(|&(_, _, ar), &(_, _, br)| ar.cmp(&br));
        vis_rand.iter().map(|&(i, cost, _)| (i, cost)).collect()
    }
}


fn find_best_year(machines: &LinkedList<Machine>) -> () {
    let get_random_year =
        || machines.iter().map(|m| (&m.code, m.get_random_visits())).collect::<Vec<_>>();
    let year = get_random_year();
    println!("{:?}", year);

    let cost_per_interval = |ref year| {

    };

    println!("Cost per month  {:?}", cost_per_interval(year));
}
