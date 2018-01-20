fn main() {
    println!("{}", comp_hm(&[3, 1, 2], &[1, 4, 9]));
    println!("{}", comp_hm(&[3, 1, 2, 1, 2], &[1, 4, 9, 4, 1]));
    println!("{}", comp_hm(&[3, 1, 2], &[1, 9]));
    println!("{}", comp_hm(&[1, 1, 1, 1], &[4, 0, 0, 0]));
}


fn comp_hm(a: &[i32], b: &[i32]) -> bool {
    use std::collections::HashMap;

    fn item_count<I>(items: I) -> HashMap<i32, i32>
        where I: std::iter::Iterator<Item = i32>
    {
        items.fold(HashMap::new(), |mut acc, item: i32| {
            *acc.entry(item).or_insert(0) += 1;
            acc
        })
    }

    item_count(a.iter().map(|x| x * x)) == item_count(b.iter().map(|x| *x))
}
