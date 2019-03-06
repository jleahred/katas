use crate::json_api::*;
use crate::position::*;
use std::collections::BTreeMap;
use std::result::Result;

pub fn add(
    mut positions: Box<Positions>,
    msg: JsonApiAdd,
) -> Result<Box<Positions>, (String, Box<Positions>)> {
    for p in positions.as_ref() {
        if p.id == msg.id {
            return Err((format!("duplicated pos on add {:?}", msg), positions));
        }
    }
    match msg.to_position() {
        Ok(pos) => {
            positions.push(pos);
            Ok(positions)
        }
        Err(e) => Err((e, positions)),
    }
}

pub fn del(
    positions: Box<Positions>,
    msg: JsonApiDel,
) -> Result<Box<Positions>, (String, Box<Positions>)> {
    let mut result: Positions = vec![];
    for p in positions.as_ref() {
        if p.id != msg.id {
            result.push(p.clone());
        }
    }
    Ok(Box::new(result))
}

pub fn gen_group_positions(positions: &[Position]) -> GroupsPos {
    let mut group_positions = BTreeMap::new();

    for p in positions {
        let mut posbyprod = group_positions
            .entry(p.sub_group.clone())
            .or_insert_with(BTreeMap::<String, PositionsByProduct>::new);
        insert_in_pos_by_prod(&mut posbyprod, p);
    }

    let mut result = vec![];

    for (group_name, gp) in group_positions {
        let mut psg = PositionsGroup {
            group: group_name,
            pos_prods: vec![],
        };
        for (_, pbp) in gp {
            psg.pos_prods.push(pbp);
        }
        result.push(psg);
    }

    result
}

fn insert_in_pos_by_prod(map_posbyprod: &mut BTreeMap<String, PositionsByProduct>, p: &Position) {
    let posbyprod = {
        let mut posbyprod = map_posbyprod
            .entry(p.isin.clone())
            .or_insert(PositionsByProduct {
                desc: p.desc.clone(),
                isin: p.isin.clone(),
                updated: p.updated.clone(),
                bids: vec![],
                asks: vec![],
            });
        posbyprod.updated = p.updated.clone();
        posbyprod
    };
    let levels = match p.side {
        Side::Bid => &mut posbyprod.bids,
        Side::Ask => &mut posbyprod.asks,
    };
    insert_level_in_levels(&p.level, levels, p.side);
}

// fn gen_pos_by_prod(positions: &[Position]) -> PosByProds {
//     use std::collections::BTreeMap;
//     let mut group_positions = BTreeMap::new();

//     for p in positions {
//         let posbyprod = {
//             let mut posbyprod =
//                 group_positions
//                     .entry(p.isin.clone())
//                     .or_insert(PositionsByProduct {
//                         desc: p.desc.clone(),
//                         isin: p.isin.clone(),
//                         updated: p.updated.clone(),
//                         bids: vec![],
//                         asks: vec![],
//                     });
//             posbyprod.updated = p.updated.clone();
//             posbyprod
//         };
//         let levels = match p.side {
//             Side::Bid => &mut posbyprod.bids,
//             Side::Ask => &mut posbyprod.asks,
//         };
//         insert_level_in_levels(&p.level, levels, p.side);
//     }

//     let mut result = vec![];

//     for (_, pbp) in group_positions {
//         result.push(pbp);
//     }

//     result
// }

fn insert_level_in_levels(level: &Level, levels: &mut Levels, side: Side) {
    let next: Box<Fn(usize, &mut Levels) -> bool> = Box::new(|_i, _levels| false); //  return
    let insert: Box<Fn(usize, &mut Levels) -> bool> = Box::new(|i, levels| {
        levels.insert(i, level.clone());
        true
    });
    let (less, greather) = match side {
        Side::Bid => (next, insert),
        Side::Ask => (insert, next),
    };
    for (i, l) in levels.clone().iter_mut().enumerate() {
        match comp_price(level.price, l.price) {
            std::cmp::Ordering::Less => {
                if less(i, levels) {
                    return;
                }
            }
            std::cmp::Ordering::Equal => {
                levels[i].qty += level.qty;
                return;
            }
            std::cmp::Ordering::Greater => {
                if greather(i, levels) {
                    return;
                }
            }
        }
    }
    levels.push(level.clone());
}

fn comp_price(p1: f64, p2: f64) -> std::cmp::Ordering {
    let normalize = |n: f64| (n * 1_000.0f64).round() as i64;

    let np1 = normalize(p1);
    let np2 = normalize(p2);

    np1.cmp(&np2)
}

//-----------------------------------------------------------
//-----------------------------------------------------------
//-----------------------------------------------------------
//-----------------------------------------------------------
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_one_level_bid() {
        let mut levels: Levels = vec![];
        let level = Level {
            price: 101.1111,
            qty: 11,
        };
        insert_level_in_levels(&level, &mut levels, Side::Bid);

        assert_eq!(levels.len(), 1);
        assert!(levels[0].qty == 11);
        assert!(comp_price(levels[0].price, 101.1111) == std::cmp::Ordering::Equal);
    }

    #[test]
    fn add_one_level_ask() {
        let mut levels: Levels = vec![];
        let level = Level {
            price: 101.1111,
            qty: 11,
        };
        insert_level_in_levels(&level, &mut levels, Side::Ask);

        assert_eq!(levels.len(), 1);
        assert!(levels[0].qty == 11);
        assert!(comp_price(levels[0].price, 101.1111) == std::cmp::Ordering::Equal);
    }

    #[test]
    fn add_two_levels_bid_first_marginal() {
        let mut levels: Levels = vec![];
        insert_level_in_levels(
            &Level {
                price: 101.1111,
                qty: 11,
            },
            &mut levels,
            Side::Bid,
        );
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 5,
            },
            &mut levels,
            Side::Bid,
        );

        assert_eq!(levels.len(), 2);
        assert!(levels[0].qty == 11);
        assert!(comp_price(levels[0].price, 101.1111) == std::cmp::Ordering::Equal);
        assert!(levels[1].qty == 5);
        assert!(comp_price(levels[1].price, 100.1111) == std::cmp::Ordering::Equal);
    }

    #[test]
    fn add_two_levels_ask_first_marginal() {
        let mut levels: Levels = vec![];
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 5,
            },
            &mut levels,
            Side::Ask,
        );
        insert_level_in_levels(
            &Level {
                price: 101.1111,
                qty: 11,
            },
            &mut levels,
            Side::Ask,
        );

        assert_eq!(levels.len(), 2);
        assert!(levels[0].qty == 5);
        assert!(comp_price(levels[0].price, 100.1111) == std::cmp::Ordering::Equal);
        assert!(levels[1].qty == 11);
        assert!(comp_price(levels[1].price, 101.1111) == std::cmp::Ordering::Equal);
    }

    //  add_two_levels_bid_second_marginal
    #[test]
    fn add_two_levels_bid_second_marginal() {
        let mut levels: Levels = vec![];
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 5,
            },
            &mut levels,
            Side::Bid,
        );
        insert_level_in_levels(
            &Level {
                price: 101.1111,
                qty: 11,
            },
            &mut levels,
            Side::Bid,
        );

        assert_eq!(levels.len(), 2);
        assert!(levels[0].qty == 11);
        assert!(comp_price(levels[0].price, 101.1111) == std::cmp::Ordering::Equal);
        assert!(levels[1].qty == 5);
        assert!(comp_price(levels[1].price, 100.1111) == std::cmp::Ordering::Equal);
    }

    #[test]
    fn add_two_levels_ask_second_marginal() {
        let mut levels: Levels = vec![];
        insert_level_in_levels(
            &Level {
                price: 101.1111,
                qty: 11,
            },
            &mut levels,
            Side::Ask,
        );
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 5,
            },
            &mut levels,
            Side::Ask,
        );

        assert_eq!(levels.len(), 2);
        assert!(levels[0].qty == 5);
        assert!(comp_price(levels[0].price, 100.1111) == std::cmp::Ordering::Equal);
        assert!(levels[1].qty == 11);
        assert!(comp_price(levels[1].price, 101.1111) == std::cmp::Ordering::Equal);
    }

    #[test]
    fn add_two_levels_bid_same_price_marginal() {
        let mut levels: Levels = vec![];
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 5,
            },
            &mut levels,
            Side::Bid,
        );
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 11,
            },
            &mut levels,
            Side::Bid,
        );

        assert_eq!(levels.len(), 1);
        assert!(levels[0].qty == 16);
        assert!(comp_price(levels[0].price, 100.1111) == std::cmp::Ordering::Equal);
    }

    #[test]
    fn add_two_levels_ask_same_price_marginal() {
        let mut levels: Levels = vec![];
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 11,
            },
            &mut levels,
            Side::Ask,
        );
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 5,
            },
            &mut levels,
            Side::Ask,
        );

        assert_eq!(levels.len(), 1);
        assert!(levels[0].qty == 16);
        assert!(comp_price(levels[0].price, 100.1111) == std::cmp::Ordering::Equal);
    }

    #[test]
    fn add_two_levels_bid_same_price_after_marginal() {
        let mut levels: Levels = vec![];
        insert_level_in_levels(
            &Level {
                price: 102.1111,
                qty: 3,
            },
            &mut levels,
            Side::Bid,
        );
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 5,
            },
            &mut levels,
            Side::Bid,
        );
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 11,
            },
            &mut levels,
            Side::Bid,
        );

        assert_eq!(levels.len(), 2);
        assert!(levels[0].qty == 3);
        assert!(comp_price(levels[0].price, 102.1111) == std::cmp::Ordering::Equal);
        assert!(levels[1].qty == 16);
        assert!(comp_price(levels[1].price, 100.1111) == std::cmp::Ordering::Equal);
    }

    #[test]
    fn add_two_levels_ask_same_price_after_marginal() {
        let mut levels: Levels = vec![];
        insert_level_in_levels(
            &Level {
                price: 99.1111,
                qty: 3,
            },
            &mut levels,
            Side::Ask,
        );
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 5,
            },
            &mut levels,
            Side::Ask,
        );
        insert_level_in_levels(
            &Level {
                price: 100.1111,
                qty: 11,
            },
            &mut levels,
            Side::Ask,
        );

        assert_eq!(levels.len(), 2);
        assert!(levels[0].qty == 3);
        assert!(comp_price(levels[0].price, 99.1111) == std::cmp::Ordering::Equal);
        assert!(levels[1].qty == 16);
        assert!(comp_price(levels[1].price, 100.1111) == std::cmp::Ordering::Equal);
    }

    //  add_three_levels_same_price_second_level_bid
    //  add_three_levels_same_price_second_level_ask

    //------------------------------------------------------------
    #[test]
    fn test_gen_positions_one_bid() {
        let positions = vec![Position {
            id: "id".to_string(),
            sub_group: "sub_group".to_string(),
            isin: "ISIN".to_string(),
            desc: "description".to_string(),
            side: Side::Bid,
            level: Level {
                price: 101.1111,
                qty: 11,
            },
            updated: "2019-02-15 15:33:15.123".to_string(),
        }];

        let group_pos = gen_group_positions(&positions);
        assert_eq!(group_pos.len(), 1);
        assert_eq!(group_pos[0].pos_prods.len(), 1);
        assert!(group_pos[0].pos_prods[0].bids.len() == 1);
        assert!(group_pos[0].pos_prods[0].asks.is_empty());
    }

    #[test]
    fn test_gen_positions_one_ask() {
        let positions = vec![Position {
            id: "id".to_string(),
            sub_group: "sub_group".to_string(),
            isin: "ISIN".to_string(),
            desc: "description".to_string(),
            side: Side::Ask,
            level: Level {
                price: 101.1111,
                qty: 11,
            },
            updated: "2019-02-15 15:33:15.123".to_string(),
        }];

        let group_pos = gen_group_positions(&positions);
        assert_eq!(group_pos.len(), 1);
        assert_eq!(group_pos[0].pos_prods.len(), 1);
        assert!(group_pos[0].pos_prods[0].asks.len() == 1);
        assert!(group_pos[0].pos_prods[0].bids.is_empty());
    }

    #[test]
    fn test_gen_positions_two() {
        let positions = vec![
            Position {
                id: "id".to_string(),
                sub_group: "sub_group".to_string(),
                isin: "ISIN".to_string(),
                desc: "description".to_string(),
                side: Side::Bid,
                level: Level {
                    price: 101.1111,
                    qty: 11,
                },
                updated: "2019-02-15 15:33:15.123".to_string(),
            },
            Position {
                id: "id2".to_string(),
                sub_group: "sub_group".to_string(),
                isin: "ISIN2".to_string(),
                desc: "description".to_string(),
                side: Side::Bid,
                level: Level {
                    price: 104.567,
                    qty: 33,
                },
                updated: "2019-02-15 15:33:15.123".to_string(),
            },
        ];

        let group_pos = gen_group_positions(&positions);
        assert_eq!(group_pos.len(), 1);
    }

    #[test]
    fn test_gen_positions_two_levels_first_marginal_bid() {
        let positions = vec![
            Position {
                id: "id".to_string(),
                sub_group: "sub_group".to_string(),
                isin: "ISIN".to_string(),
                desc: "description".to_string(),
                side: Side::Bid,
                level: Level {
                    price: 105.1111,
                    qty: 11,
                },
                updated: "2019-02-15 15:33:15.123".to_string(),
            },
            Position {
                id: "id2".to_string(),
                sub_group: "sub_group".to_string(),
                isin: "ISIN".to_string(),
                desc: "description".to_string(),
                side: Side::Bid,
                level: Level {
                    price: 104.567,
                    qty: 33,
                },
                updated: "2019-02-15 15:33:15.123".to_string(),
            },
        ];

        let group_pos = gen_group_positions(&positions);

        assert_eq!(group_pos.len(), 1);
        assert_eq!(group_pos[0].pos_prods.len(), 1);
        assert!(group_pos[0].pos_prods[0].bids.len() == 2);
        assert!(group_pos[0].pos_prods[0].asks.is_empty());
        assert!(
            comp_price(group_pos[0].pos_prods[0].bids[0].price, 105.111)
                == std::cmp::Ordering::Equal
        );
        assert_eq!(group_pos[0].pos_prods[0].bids[0].qty, 11);
    }

}
