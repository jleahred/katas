extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

use serde_json::Error;


fn main() {
    println!("{}", typed_example().is_ok());
    println!("{}", renamed_example().is_ok());
}



fn typed_example() -> Result<(), Error> {
    #[derive(Serialize, Deserialize, Debug)]
    struct Person {
        name: String,
        age: u8,
        phones: Vec<String>,
        optional: Option<String>,
    }
    let data = r#"{
                    "name": "John Doe",
                    "age": 43,
                    "phones": [
                      "+44 1234567",
                      "+44 2345678"
                    ],
                    "optional": null
                  }"#;

    let p: Person = serde_json::from_str(data)?;
    println!("{:?}", p);
    Ok(())
}


fn renamed_example() -> Result<(), Error> {
    #[derive(Serialize, Deserialize, Debug)]
    struct Person {
        #[serde(rename = "nm")] name: String,
        #[serde(rename = "ag")] age: u8,
        #[serde(rename = "ph")] phones: Vec<String>,
        #[serde(rename = "op")] optional: Option<String>,
    }
    let data = r#"{
                    "nm": "John Doe",
                    "ag": 43,
                    "ph": [
                      "+44 1234567",
                      "+44 2345678"
                    ],
                    "op": null
                  }"#;

    let p: Person = serde_json::from_str(data)?;
    println!("{:?}", p);
    Ok(())
}
