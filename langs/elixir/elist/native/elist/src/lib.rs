#[rustler::nif]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif]
fn fix_parse(msg: &str) -> Vec<(String, String)> {
    msg.split('|')
        .filter(|s| !s.is_empty())
        .map(|kv| {
            let mut it = kv.split('=');
            match (it.next(), it.next()) {
                (Some(tag), Some(value)) => (tag.to_owned(), value.to_owned()),
                _ => panic!("format error"),
            }
        })
        .collect()
}

rustler::init!("Elixir.Elist", [add, fix_parse]);
