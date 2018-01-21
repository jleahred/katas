extern crate regex;

fn main() {
    println!("Hello, world!");
}


struct Hour(i8);
struct Min(i8);
struct Second(i8);
enum AmPm {
    am,
    pm,
}

struct Time12 {
    hour: Hour,
    min: Min,
    second: Second,
    am_pm: AmPm,
}


fn parse_str_t12(st12: &str) -> Option<Time12> {
    use regex::Regex;
    let regex_h12 = Regex::new(r"^([01][09])\:([05][09])\:([05][09])([AM|PM])$").unwrap();

    let caps = regex_h12.captures(st12).unwrap();
    if caps.len() != 4 {
        return None;
    }

    let am_pm = match caps.get(4).unwrap().as_str() {
        "AM" => AmPm::am,
        "PM" => AmPm::pm,
        _ => panic!("unreachable code"),
    };

    Some(Time12 {
        hour: Hour(caps.get(1).unwrap().as_str().parse().unwrap()),
        min: Min(caps.get(1).unwrap().as_str().parse().unwrap()),
        second: Second(caps.get(1).unwrap().as_str().parse().unwrap()),
        am_pm: am_pm,
    })
}
