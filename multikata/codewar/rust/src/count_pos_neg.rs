fn count_positives_sum_negatives(input: Vec<i32>) -> Vec<i32> {
    if input == vec![] {
        vec![]
    } else {
        let (pos, neg): (Vec<i32>, Vec<i32>) = input.iter().partition(|&x| *x > 0);
        vec![pos.iter().count() as i32, neg.iter().sum()]
    }
}
// fn count_positives_sum_negatives(input: Vec<i32>) -> Vec<i32> {
//     use std::cmp::Ordering;

//     let (count_p, sum_neg) = input
//         .iter()
//         .fold((0, 0), |(count_p, sum_neg), x| match x.cmp(&0) {
//             Ordering::Equal => (count_p, sum_neg),
//             Ordering::Greater => (count_p + 1, sum_neg),
//             Ordering::Less => (count_p, sum_neg + x),
//         });
//     match (count_p, sum_neg) {
//         (0, 0) => vec![],
//         _ => vec![count_p, sum_neg],
//     }
// }

#[test]
fn returns_expected() {
    let test_data1 = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, -11, -12, -13, -14, -15];
    let expected1 = vec![10, -65];
    assert_eq!(count_positives_sum_negatives(test_data1), expected1);
}
