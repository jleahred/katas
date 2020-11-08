extern crate serde;
extern crate tera;

use serde::{Deserialize, Serialize};
use tera::Tera;

#[derive(Serialize, Deserialize)]
struct Address {
    street: String,
    city: String,
}

#[derive(Serialize, Deserialize)]
struct SuperAddress {
    address: Address,
}

fn main() {
    let address = SuperAddress {
        address: Address {
            street: "calle".to_string(),
            city: "ciudad".to_string(),
        },
    };
    let result = Tera::one_off(
        "{{ __tera_context }}
     {% set var = 2 -%}   
    c:  {{ address.city }}  {{ var }}
        ",
        &tera::Context::from_serialize(address).unwrap(),
        false,
    )
    .unwrap();

    println!("{}", result);
}
