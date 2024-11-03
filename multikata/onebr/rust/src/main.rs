// use rayon::prelude::*;
use std::collections::BTreeMap;
use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug)]
struct StationValues {
    minimum: f32,
    maximum: f32,
    sum: f32,
    count: i32,
}

fn main() -> io::Result<()> {
    let file = File::open("../measurements.txt")?;
    let reader = io::BufReader::new(file);

    let result = reader
        .lines()
        // .par_iter()
        .filter_map(Result::ok)
        .take(10_000_000)
        .map(|l| match l.split(";").collect::<Vec<&str>>()[..] {
            [station, value] => (station.to_string(), value.parse::<f32>().unwrap()),
            _ => panic!("inconsistent"),
        })
        .fold(BTreeMap::new(), |mut acc, (station, value)| {
            acc.entry(station)
                .and_modify(|prev: &mut StationValues| {
                    prev.minimum = prev.minimum.min(value);
                    prev.maximum = prev.maximum.max(value);
                    prev.sum += value;
                    prev.count += 1
                })
                .or_insert(StationValues {
                    minimum: value,
                    maximum: value,
                    sum: value,
                    count: 1,
                });
            acc
        });

    println!("{:?}", result);

    Ok(())
}
