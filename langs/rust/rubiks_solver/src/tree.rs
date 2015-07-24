use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::LinkedList;
use cube;
use config;


/* brutal force cost...

    for cubes 4x4x4

    bfc(depth) = moves

    bfc(1) = 24
    bfc(2) = 24 + 24^2
    bfc(3) = bfc(3-1) + 24^3
    ...
    bfc(n) = bfc(n-1) + 24^n

    bfc(n) = bfc(n-2) + 24^(n-1) + 24^n
    bfc(n) = bfc(n-3) + 24^(n-2) + 24^(n-1) + 24^(n-0)

    bfc(n) = 24(1+24*(1+24*(1+24*...)))

*/



#[derive(Debug, Clone, Copy)]
pub struct Found {
    depth       : u8,
    iterations  : u64,
}

fn get_better(l : &Option<Found>, r : &Found) -> Option<Found> {
    match *l {
        Some(found_l) => {
            if found_l.iterations < r.iterations { Some(found_l) }
            else { Some(*r) }
        },
        None        => Some(*r)
    }
}


impl fmt::Display for Found {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "found.depth:  {}\n", self.depth));
        try!(write!(f, "found.iter:   {}", self.iterations));
        write!(f, "")
    }
}


//#[derive(Debug, Clone, Copy)]
#[derive(Debug, Clone)]
pub struct Status {
    pub depth           : u8,
    pub max_depth       : u8,
    pub iterations      : u64,

    pub best_found      : Option<Found>,

    shared_current_path    : Rc<RefCell<LinkedList<cube::rot::Item>>>,     //  this is an optimization
}

impl Status {
    fn next_iteration(&self, rot: &cube::rot::Item) -> Status {
        let mut result = self.clone();
        result.iterations += 1;
        result.depth += 1;
        result.shared_current_path.borrow_mut().push_back(*rot);
        result
    }

    fn new_update(&self, best_found : &Option<Found>,  iterations : u64) -> Status {
        let mut result = self.clone();
        result.best_found = *best_found;
        result.iterations += iterations;
        result.shared_current_path.borrow_mut().pop_back();
        result
    }
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.best_found {
            Some(best_found)    =>  try!(write!(f, "---------\nbest_found:  {}\n------------\n", best_found)),
            None                =>  try!(write!(f, "NOT FOUND\n")),
        }
        try!(write!(f, "depth:       {}\n", self.depth));
        try!(write!(f, "iterations:  {}\n", self.iterations));
        try!(write!(f, "max_depth:   {}\n", self.max_depth));
        write!(f, "")
    }
}




pub fn explore(origin : &cube::Sides, end : &cube::Sides, max_depth : u8) -> Status
{
    internal_explore(origin, end, Status{   depth:              0,
                                            max_depth:          max_depth,
                                            iterations:         0,
                                            best_found:         None,
                                            shared_current_path:   Rc::new(RefCell::new(LinkedList::new()))
                                            })
}

fn internal_explore(origin : &cube::Sides, end : &cube::Sides, status : Status) -> Status
{
    //println!("depth: {}", status.depth);
    //println!("depth: {}", origin);
    //println!("current_path: {:?}\n", status.shared_current_path);

    let mut acc_iterations = 0;
    let mut local_best_found = status.best_found;
    if cube::equivalent_end(origin, end) {
        let mut result = status.clone();
        result.best_found = Some(Found{ depth:          status.depth,
                                        iterations:     status.iterations,
                                    });

        println!("Found...... {}\n", result);
        println!("{}\n", &origin);
        result
    } else {
        if status.depth < status.max_depth {
            //  TODO:  continue here

            let mut iterate_orient_dir = |orientation : cube::rot::Orient, direction : cube::rot::Dir| -> () {
                for i in 0.. config::SIZE {
                    let next_move = &cube::rot::Item(
                            orientation,
                            direction,
                            i);
                    let next = origin.get_rotation(next_move);
                    let result = internal_explore(&next, end, status.next_iteration(next_move));
                    match result.best_found {
                        Some(located_best_found)    => local_best_found = get_better(&local_best_found, &located_best_found),
                        None                        => acc_iterations += result.iterations - status.iterations,
                    }
                }
            };

            {
                use cube::rot::{Orient, Dir};
                iterate_orient_dir(Orient::Horizontal,  Dir::Plus);
                iterate_orient_dir(Orient::Horizontal,  Dir::Minus);

                iterate_orient_dir(Orient::Vertical,    Dir::Plus);
                iterate_orient_dir(Orient::Vertical,    Dir::Minus);

                iterate_orient_dir(Orient::Front,       Dir::Plus);
                iterate_orient_dir(Orient::Front,       Dir::Minus);
            }
        }
        status.new_update(&local_best_found, acc_iterations)
    }
}


/*
#[test]
fn test_tree_simple() {
    let init : cube::Sides = cube::create_from_strings(
                     ["0000",
                      "0000",
                      "0000",
                      "0000",
                 "0001 0000 0000",
                 "0002 0000 0000",
                 "0003 0000 0000",
                 "0004 0000 0000",
                      "0000",
                      "0000",
                      "0000",
                      "0000",

                      "0000",
                      "0000",
                      "0000",
                      "0000"]
        );

    let end : cube::Sides = cube::create_from_strings(
                    ["0000",
                     "0000",
                     "0000",
                     "4321",
                "0000 0000 0000",
                "0000 0000 0000",
                "0000 0000 0000",
                "0000 0000 0000",
                     "0000",
                     "0000",
                     "0000",
                     "0000",

                     "0000",
                     "0000",
                     "0000",
                     "0000"]
                );

}
*/
