use std::fmt;
use std::collections::LinkedList;
use std::cell::RefCell;
use cube;
use config;

mod opts;


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

fn get_better(l : Option<Found>, r : &Found) -> Option<Found> {
    match l {
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


#[derive(Debug, Clone, Copy)]
pub struct RotationPosition {
    pub rot     :   cube::rot::Item,
    pub position:   cube::Sides,
}



#[derive(Debug, Clone)]
pub struct PunningStats {
    pub depth_less_found        : u64,
    pub punning_3_consecutives  : u64,
    pub repeated_in_path        : u64,
    pub direction_higher_level  : u64,
    pub inverse_move            : u64,
}

fn empty_punning_stats () -> PunningStats {
    PunningStats {
                            depth_less_found: 0,
                            punning_3_consecutives: 0,
                            repeated_in_path: 0,
                            direction_higher_level:0,
                            inverse_move: 0,
                        }
}


//#[derive(Debug, Clone, Copy)]

#[derive(Clone)]
pub struct  Stats {
    pub iterations      : u64,
    pub punning         : RefCell<PunningStats>,
}

#[derive(Clone)]
pub struct Config {
    pub max_depth       : u8,
    pub generate_cache  : bool,
}


#[derive(Clone)]
pub struct Status {
    pub stats           : Stats,
    pub config          : Config,

    pub depth           : u8,

    pub best_found      : Option<Found>,

    current_path        : LinkedList<RotationPosition>,
    pub best_solution   : LinkedList<RotationPosition>,

    pub last_moves      : RefCell<opts::cache_last_moves::PosMoves>
}



impl Status {
    fn push(&mut self, rot_pos: &RotationPosition) -> &mut Status {
        self.stats.iterations += 1;
        self.depth += 1;
        self.current_path.push_back(*rot_pos);
        self
    }

    fn pop(&mut self) -> &mut Status {
        self.current_path.pop_back();
        self.depth -= 1;
        self
    }
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.best_found {
            Some(best_found)    =>  try!(write!(f, "---------\nbest_found:  {}\n------------\n", best_found)),
            None                =>  try!(write!(f, "NOT FOUND\n")),
        }
        try!(write!(f, "current depth:  {}\n", self.depth));
        try!(write!(f, "iterations:     {}\n", self.stats.iterations));
        try!(write!(f, "max_depth:      {}\n", self.config.max_depth));
        write!(f, "")
    }
}




pub fn explore(origin : &cube::Sides, end : &cube::Sides, max_depth : u8) -> Box<Status>
{
    let mut status  = Box::new(Status{
                                    stats: Stats {
                                        iterations      : 0u64,
                                        punning         : RefCell::new(empty_punning_stats()),
                                    },
                                    config: Config {
                                        max_depth:          max_depth,
                                        generate_cache:     false,
                                    },
                                    depth:              0,
                                    best_found:         None,
                                    current_path:       LinkedList::new(),
                                    best_solution:      LinkedList::new(),
                                    last_moves:         RefCell::new(opts::cache_last_moves::PosMoves::new()),
                                });

    println!("Generating cache last moves...");
    status.config.generate_cache = true;
    status.config.max_depth = 5;
    internal_explore(end, origin, &mut status);
    println!("Cache last moves generated. size: {}", status.last_moves.borrow_mut().len());

    println!("Exploring...");
    status.config.generate_cache = false;
    status.config.max_depth = max_depth;
    internal_explore(origin, end, &mut status);

    status
}

fn internal_explore(origin : &cube::Sides, end : &cube::Sides, status : &mut Status) -> ()
{
    let update_best_solution =  |   status: &mut Status,
                                    cache_last_moves: &Option<LinkedList<RotationPosition>>|  -> () {
        print!("FOUND!!!!!!!!!!!!!!!!!!!!!!!!!! {}", status.depth);
        status.best_found = Some(
                                        Found{ depth:   status.depth,
                                        iterations:     status.stats.iterations, });
        status.best_solution.clear();
        for path in status.current_path.iter() {
            status.best_solution.push_back(*path);
        }
        match *cache_last_moves {
            Some(ref last_moves)    => {
                print!("+{}", last_moves.len());
                for it in last_moves.iter().rev() {
                    status.best_solution.push_back(RotationPosition {
                        rot:        it.rot.get_reverse(),
                        position:   it.position,
                    });
                }
            },
            None                => (),
        };
        for mv in status.best_solution.iter() {
            print!(" {}", mv.rot);
        }
        println!("");
    };
    let update_best_found = |   status: &mut Status,
                                cache_last_moves: &Option<LinkedList<RotationPosition>>| {
        match status.best_found {
            Some(prev_best_found)     => {
                if status.depth < prev_best_found.depth  {
                    update_best_solution(status, cache_last_moves);
                }
            }
            None            => update_best_solution(status, cache_last_moves),
        }
    };
    if status.config.generate_cache==false && cube::equivalent_end(origin, end) {
        update_best_found(status, &None);
        return;
    } else {
        //if status.depth >= status.config.max_depth  {
            if  status.config.generate_cache {
                if status.depth >= status.config.max_depth {
                    status.last_moves.borrow_mut().add(origin, &status.current_path);
                }
            } else {
                //  last moves optimization
                let ofound = status.last_moves.borrow().find(origin);
                if ofound.is_none() == false {
                    update_best_found(status, &ofound);
                    return;
                }
            }
        //}
        //else {
        if status.depth < status.config.max_depth {
            let mut iterate_orient_dir = |  orientation : cube::rot::Orient,
                                            direction : cube::rot::Dir| -> () {
                for i in 0.. config::SIZE {
                    let next_move = &cube::rot::Item(
                            orientation,
                            direction,
                            i);

                    if opts::before_move::depth_bigger_or_equal_best_sol(status, &mut status.stats.punning.borrow_mut()) {
                        break;
                    }
                    if opts::before_move::inverse_move(&next_move, status, &mut status.stats.punning.borrow_mut()) {
                        continue;
                    }
                    if opts::before_move::three_consecutive_moves(&next_move, status, &mut status.stats.punning.borrow_mut()) {
                        continue;
                    }
                    if opts::before_move::same_direction_higher_level(&next_move, status, &mut status.stats.punning.borrow_mut()) {
                        continue;
                    }

                    let next = origin.get_rotation(next_move);
                    let rot_pos = RotationPosition{ rot: *next_move, position: next };

                    /*if opts::after_move::pos_equal2current_path(&next, status, &mut status.punning_stats.borrow_mut()) {
                        continue;
                    }*/

                    internal_explore(&next, end, status.push(&rot_pos));
                    match status.best_found {
                        Some(located_best_found)    => status.best_found = get_better(status.best_found, &located_best_found),
                        None                        => (),
                    };
                    status.pop();
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
