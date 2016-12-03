#[derive(Debug, Clone, Copy)]
pub enum Distance {
    Meters(f64),
    Centimeters(f64),
    Millimeters(f64),
    Killometers(f64),
}  //  << ----------   new unit here  *****************

impl Distance {
    fn unwrap_f64_meters(&self) -> f64 {
        match self {
            &Distance::Meters(i) => i,
            &Distance::Centimeters(i) => i / 100.,
            &Distance::Millimeters(i) => i / 1_000.,
            &Distance::Killometers(i) => i * 1_000.,
            //  <<---------  new unit here  **************
        }
    }
}


impl ::std::ops::Add for Distance {
    type Output = Distance;

    fn add(self, r: Distance) -> Distance {
        let self_im = self.unwrap_f64_meters();
        let r_im = r.unwrap_f64_meters();
        Distance::Meters(self_im + r_im)
    }
}

impl ::std::ops::Sub for Distance {
    type Output = Distance;

    fn sub(self, r: Distance) -> Distance {
        self + (-1. * r)
    }
}


impl ::std::ops::Mul<Distance> for f64 {
    type Output = Distance;

    fn mul(self, r: Distance) -> Distance {
        let r_im = r.unwrap_f64_meters();
        Distance::Meters(r_im * self)
    }
}

impl ::std::ops::Mul<f64> for Distance {
    type Output = Distance;

    fn mul(self, r: f64) -> Distance {
        r * self
    }
}
