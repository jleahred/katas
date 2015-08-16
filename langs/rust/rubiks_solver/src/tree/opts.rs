
pub mod before_move {
    use super::super::Status;
    use super::super::PunningStats;
    use super::super::super::cube::rot;

    pub fn depth_bigger_or_equal_best_sol(status: & Status, punning_stats : &mut PunningStats) -> bool {
        match status.best_found {
            Some(found)     =>  if found.depth <= status.depth {
                                    punning_stats.depth_less_found += 1; true
                                }
                                else { false },
            None            =>  false
        }
    }

    pub fn three_consecutive_moves(next_move: &rot::Item, status: &Status, punning_stats : &mut PunningStats) -> bool {
        if status.current_path.len()>=2 {
            let mut it = status.current_path.iter().rev();
            let last_move = it.next().unwrap().rot;
            let prev_move = it.next().unwrap().rot;

            if *next_move==last_move &&  *next_move==prev_move {
                punning_stats.punning_3_consecutives += 1;
                true
            }
            else { false }
        }
        else { false }
    }

    pub fn same_direction_higher_level(next_move: &rot::Item, status: &Status, punning_stats : &mut PunningStats) -> bool {
        match status.current_path.back() {
            Some(rot_position) => {
                let rot::Item(last_orient, _dir, last_level) = rot_position.rot;
                let rot::Item(next_orient, _dir, next_level) = *next_move;
                if next_orient == last_orient  &&  next_level < last_level {
                    punning_stats.direction_higher_level += 1;
                    true
                } else { false }
            },
            None => false
        }
    }

    pub fn inverse_move(next_move: &rot::Item, status: &Status, punning_stats : &mut PunningStats) -> bool {
        match status.current_path.back() {
            Some(rot_position) => {
                let rot::Item(last_orient, last_dir, last_level) = rot_position.rot;
                let rot::Item(next_orient, next_dir, next_level) = *next_move;
                if next_orient == last_orient
                &&  next_level == last_level
                &&  next_dir != last_dir {
                    punning_stats.inverse_move += 1;
                    true
                } else { false }
            },
            None => false
        }
    }
}


pub mod after_move {
    /*
    use super::super::Status;
    use super::super::PunningStats;
    use super::super::super::cube;
    pub fn pos_equal2current_path(sides: &cube::Sides, status: &Status, punning_stats : &mut PunningStats) -> bool {
        for it in status.current_path.iter().rev() {
            if it.position == *sides {
                punning_stats.repeated_in_path += 1;
                return true
            }
        }
        false
    }
*/
}


pub mod cache_last_moves {
    use cube;
    use std::collections::LinkedList;
    use std::collections::HashMap;
    use tree::RotationPosition;


    #[derive(Clone)]
    pub struct PosMoves {
        pos_movs : HashMap<
                    cube::Sides,                    //  position
                    LinkedList<RotationPosition>,
                    //LinkedList<cube::rot::Item>,
                >,    //  rotations to end
    }

    impl PosMoves {
        pub fn new() -> PosMoves{
            PosMoves {
                pos_movs : HashMap::new(),
            }
        }

        pub fn add(&mut self, pos: &cube::Sides, path:  &LinkedList<RotationPosition>) {
            let ref_found = self.pos_movs.entry(*pos).or_insert(path.clone());
            if ref_found.len() > path.len() {
                *ref_found = path.clone();
            }
        }

        pub fn len(&self) -> usize {
            self.pos_movs.len()
        }

        pub fn find(&self, pos : &cube::Sides) -> Option<LinkedList<RotationPosition>> {
            match self.pos_movs.get(pos) {
                Some(rot_pos)   =>   Some(rot_pos.clone()),
                None            =>  None
            }
        }
    }
}


#[test]
fn test_insert_cache() {
    use super::super::cube;
    use std::collections::LinkedList;

    let posa : cube::Sides = cube::create_from_strings(
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
    let posb : cube::Sides = cube::create_from_strings(
                     ["1111",
                      "1111",
                      "1111",
                      "1111",
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

    let mut pos_moves = cache_last_moves::PosMoves::new();
    let moves = LinkedList::<cube::rot::Item>::new();

    pos_moves.add(posa, &moves);
    assert_eq!(pos_moves.len(), 1);

    pos_moves.add(posa, &moves);
    assert_eq!(pos_moves.len(), 1);

    pos_moves.add(posb, &moves);
    assert_eq!(pos_moves.len(), 2);

    pos_moves.add(posa, &moves);
    assert_eq!(pos_moves.len(), 2);
}
