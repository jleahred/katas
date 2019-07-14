extern crate uom;

use uom::si::f64::*;
use uom::si::mass::kilogram;
use uom::si::velocity::meter_per_second;
// use uom::si::Unit;

fn main() {
    println!("Calculating collisions...");

    let digits = 10;

    let big_mass = Mass::new::<kilogram>(10f64.powi((digits - 1) * 2));
    let mut st = get_init_status(big_mass);
    while will_be_collision(&st) {
        st = process_collision(st);
    }
    println!("Number of collisions... {}", st.collisions);
}

fn process_collision(mut st: Status) -> Status {
    if st.small_speed < Velocity::new::<meter_per_second>(0.0) {
        //  against wall
        st.small_speed *= -1.0;
    } else {
        //  collision two objects
        //
        //  v1f = (2*m2*v2i + (m1-m2)*v1i) / (m1+m2)
        //  v2f = (2*m1*v1i + (m2-m1)*v2i) / (m1+m2)
        //  v<x><y> -> vel x:[obj1|obj2] y:[initial|final]
        //
        //  In our case small mass is 1
        //  small 1, big 2
        //  small_vf = (2.0*big_mass*big_vi + (1.0-big_mass)*small_vi) / (1.0+big_mass)
        //  big_vf = (2.0*small_vi + (big_mass-1.0)*big_vi) / (1.0+big_mass)

        let small_vf = (2.0 * st.big_mass * st.big_speed
            + (Mass::new::<kilogram>(1.0) - st.big_mass) * st.small_speed)
            / (Mass::new::<kilogram>(1.0) + st.big_mass);
        let big_vf = (2.0 * Mass::new::<kilogram>(1.0) * st.small_speed
            + (st.big_mass - Mass::new::<kilogram>(1.0)) * st.big_speed)
            / (Mass::new::<kilogram>(1.0) + st.big_mass);

        st.big_speed = big_vf;
        st.small_speed = small_vf;
    }

    st.collisions += 1;
    st
}

fn will_be_collision(st: &Status) -> bool {
    if st.small_speed < Velocity::new::<meter_per_second>(0.0) {
        true
    } else {
        st.small_speed > st.big_speed
    }
}

#[derive(Debug)]
struct Status {
    collisions: u64,
    big_mass: Mass, //  the small mass will be allways 1.0
    big_speed: Velocity,
    small_speed: Velocity,
}

fn get_init_status(big_mass: Mass) -> Status {
    Status {
        collisions: 0,
        big_mass,
        big_speed: Velocity::new::<meter_per_second>(-1.0),
        small_speed: Velocity::new::<meter_per_second>(-0.0),
    }
}
