use std::collections::linked_list::LinkedList;

macro_rules! list {
    ($( [$num: expr,  $minuts: expr ]),*) => {{
         let mut list = ::std::collections::LinkedList::new();
         $( list.push_back(Visit_info{num: $num, minuts: $minuts}); )*
         list
    }}
}

macro_rules! machine_list {
    ( $( { $mcode: expr, [ $( ($num: expr,  $minuts: expr) ),* ] } ),* ) => {{
        let mut machines = ::std::collections::LinkedList::<Machine>::new();
        $( 
            let mut lvisit_info = ::std::collections::LinkedList::<Visit_info>::new();
            $( lvisit_info.push_back(Visit_info{num: $num, minuts: $minuts, visited:  0}); )*
            let machine = Machine{code: $mcode.to_string(), visit_info: lvisit_info};
            machines.push_back(machine);
        )*
        machines
    }}
}



#[derive(Debug)]
struct Visit_info {
    num: i16,
    minuts: i16,

    visited: i16,
}


#[derive(Debug)]
struct Machine {
    code: std::string::String,
    visit_info: LinkedList<Visit_info>,
}



fn main() {
    let machines = machine_list!{
        { "a", [( 9, 15), ( 2, 8), ( 1, 12)] },
        { "b", [( 5, 15), ( 4, 8), ( 3, 12)] }
    };
    let groups = 12;


    for m in machines {
        println!("{:?}", m);
    }
}
