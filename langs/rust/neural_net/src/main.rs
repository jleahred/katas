extern crate neuroflow;

use neuroflow::activators::Type::Tanh;
// use neuroflow::activators::Type::Sigmoid;
use neuroflow::data::DataSet;
use neuroflow::FeedForward;

// //  params
// const TRAINING_REPETITIONS: i64 = 5_000_000;
// const FROM: usize = 0;
// const TO: usize = 99;
// fn transf(i: i32) -> i32 {
//     i + 1
// }

// fn main() {
//     /*
//         Define neural network with 20 neuron in input layers. Each for decimal digit, two digits.
//         Network contains 4 hidden layers.
//         It retursn 20 neurons, two digits base 10
//     */
//     let mut nn = FeedForward::new(&[1, 100, 100, 100, 20]);
//     let training_data = {
//         (FROM..TO).fold(DataSet::new(), |mut acc, n| {
//             let i = n as i32;
//             if i != 5 {
//                 acc.push(&[i as f64 / 100.], &number_to_neurons(transf(i)));
//             }
//             acc
//         })
//     };

//     // Here, we set necessary parameters and train neural network by our DataSet
//     println!("START training...");
//     nn.activation(Tanh)
//         .learning_rate(0.001)
//         .train(&training_data, TRAINING_REPETITIONS);
//     println!("END training...");

//     (FROM..TO).for_each(|n| {
//         let i = n as i32;
//         let mut number_from_neurons = |n| neurons_to_number(nn.calc(&[n as f64 / 100.0]));
//         if number_from_neurons(i) != transf(i) {
//             println!(
//                 "ERROR for [{:?}] -> [{:?}]",
//                 i as f64,
//                 number_from_neurons(i)
//             );
//         }
//     });
// }

// fn number_to_neurons(num: i32) -> Vec<f64> {
//     let d2 = num - (num / 10) * 10;
//     let d1 = (num - d2) / 10;

//     let mut neurons = vec![0.0; 20];
//     neurons[d1 as usize] = 1.0;
//     neurons[(d2 + 10) as usize] = 1.0;
//     neurons
// }

// fn neurons_to_number(neurons: &[f64]) -> i32 {
//     let neurons2digit = |n: &[f64], from, to| {
//         n[from..to]
//             .iter()
//             .fold((0., 0, 0), |acc, &x| {
//                 //  acc -> (maximun found, counter, max pos found)
//                 if acc.0 < x {
//                     (x, acc.1 + 1, acc.1)
//                 } else {
//                     (acc.0, acc.1 + 1, acc.2)
//                 }
//             })
//             .2
//     };
//     neurons2digit(neurons, 0, 10) * 10 + neurons2digit(neurons, 10, 20)
// }

// #[test]
// fn test_number_neurons() {
//     (0..100).for_each(|i| {
//         let neurons = number_to_neurons(i);
//         let end_n = neurons_to_number(&neurons);
//         assert_eq!(i, end_n);
//     });
// }

// //  params
// const TRAINING_REPETITIONS: i64 = 1_000_000;
// const FROM: usize = 0;
// const TO: usize = 99;
// fn transf(i: i32) -> i32 {
//     i + 1
// }

// fn main() {
//     /*
//         Define neural network with 20 neuron in input layers. Each for decimal digit, two digits.
//         Network contains 4 hidden layers.
//         It retursn 20 neurons, two digits base 10
//     */
//     let mut nn = FeedForward::new(&[20, 30, 100, 100, 30, 20]);
//     let training_data = {
//         (FROM..TO).fold(DataSet::new(), |mut acc, n| {
//             let i = n as i32;
//             if i != 5 {
//                 acc.push(&number_to_neurons(i), &number_to_neurons(transf(i)));
//             }
//             acc
//         })
//     };

//     // Here, we set necessary parameters and train neural network by our DataSet
//     println!("START training...");
//     nn.activation(Tanh)
//         .learning_rate(0.001)
//         .train(&training_data, TRAINING_REPETITIONS);
//     println!("END training...");

//     (FROM..TO).for_each(|n| {
//         let i = n as i32;
//         let mut number_from_neurons = |n| neurons_to_number(nn.calc(&number_to_neurons(n)));
//         if number_from_neurons(i) != transf(i) {
//             println!(
//                 "ERROR for [{:?}] -> [{:?}]",
//                 i as f64,
//                 neurons_to_number(nn.calc(&number_to_neurons(i)))
//             );
//         }
//     });
// }

// fn number_to_neurons(num: i32) -> Vec<f64> {
//     let d2 = num - (num / 10) * 10;
//     let d1 = (num - d2) / 10;

//     let mut neurons = vec![0.0; 20];
//     neurons[d1 as usize] = 1.0;
//     neurons[(d2 + 10) as usize] = 1.0;
//     neurons
// }

// fn neurons_to_number(neurons: &[f64]) -> i32 {
//     let neurons2digit = |n: &[f64], from, to| {
//         n[from..to]
//             .iter()
//             .fold((0., 0, 0), |acc, &x| {
//                 //  acc -> (maximun found, counter, max pos found)
//                 if acc.0 < x {
//                     (x, acc.1 + 1, acc.1)
//                 } else {
//                     (acc.0, acc.1 + 1, acc.2)
//                 }
//             })
//             .2
//     };
//     neurons2digit(neurons, 0, 10) * 10 + neurons2digit(neurons, 10, 20)
// }

// #[test]
// fn test_number_neurons() {
//     (0..100).for_each(|i| {
//         let neurons = number_to_neurons(i);
//         let end_n = neurons_to_number(&neurons);
//         assert_eq!(i, end_n);
//     });
// }

// fn main() {
//     /*
//         Define neural network with 1 neuron in input layers. Network contains 4 hidden layers.
//         And, such as our function returns single value, it is reasonable TO have 1 neuron in the output layer.
//     */
//     let mut nn = FeedForward::new(&[1, 8, 8, 8, 8, 1]);
//     // let mut nn = FeedForward::new(&[1, 2, 1]);
//     /*
//         Define DataSet.
//         DataSet is the Type that significantly simplifies work with neural network.
//         Majority of its functionality is still under development :(
//     */
//     let training_data = {
//         (0..10).fold(DataSet::new(), |mut acc, i| {
//             if i != 5 || i == 4 {
//                 acc.push(&[i as f64 / 10.], &[(i * 2) as f64 / 10.0]);
//             }
//             acc
//         })
//     };
//     // Here, we set necessary parameters and train neural network by our DataSet with 50 000 iterations
//     nn.activation(Tanh)
//         .learning_rate(0.001)
//         .train(&training_data, 1_000_000);

//     (0..10).for_each(|i| {
//         println!(
//             "for [{:?}] -> [{:?}]",
//             i as f64,
//             (nn.calc(&[i as f64 / 10.])[0] * 10.).round()
//         );
//     });
// }

fn main() {
    /*
        Define neural network with 1 neuron in input layers. Network contains 4 hidden layers.
        And, such as our function returns single value, it is reasonable TO have 1 neuron in the output layer.
    */
    let mut nn = FeedForward::new(&[1, 8, 8, 8, 8, 1]);
    // let mut nn = FeedForward::new(&[1, 2, 1]);
    /*
        Define DataSet.
        DataSet is the Type that significantly simplifies work with neural network.
        Majority of its functionality is still under development :(
    */
    let training_data = {
        (0..10).fold(DataSet::new(), |mut acc, i| {
            if i != 5 || i == 4 {
                acc.push(&[i as f64 / 10.], &[(i as f64).sqrt() / 10.]);
            }
            acc
        })
    };
    // Here, we set necessary parameters and train neural network by our DataSet with 50 000 iterations
    nn.activation(Tanh)
        .learning_rate(0.001)
        .train(&training_data, 3_000_000);

    (0..10).for_each(|i| {
        println!(
            "for [{:?}] -> [{:?}]  real: [{:?}]",
            i as f64,
            ((nn.calc(&[i as f64 / 10.])[0] * 10.) * 10.).round() / 10.,
            ((i as f64).sqrt() * 10.).round() / 10.
        );
    });
}

// fn main() {
//     /*
//         Define neural network with 1 neuron in input layers. Network contains 4 hidden layers.
//         And, such as our function returns single value, it is reasonable TO have 1 neuron in the output layer.
//     */
//     let mut nn = FeedForward::new(&[1, 8, 8, 8, 8, 1]);
//     // let mut nn = FeedForward::new(&[1, 2, 1]);
//     /*
//         Define DataSet.
//         DataSet is the Type that significantly simplifies work with neural network.
//         Majority of its functionality is still under development :(
//     */
//     let training_data = {
//         (0..10).fold(DataSet::new(), |mut acc, i| {
//             if i != 5 || i == 4 {
//                 acc.push(&[i as f64 / 10.], &[(i + 1) as f64 / 10.0]);
//             }
//             acc
//         })
//     };
//     // Here, we set necessary parameters and train neural network by our DataSet with 50 000 iterations
//     nn.activation(Tanh)
//         .learning_rate(0.001)
//         .train(&training_data, 1_000_000);

//     (0..10).for_each(|i| {
//         println!(
//             "for [{:?}] -> [{:?}]",
//             i as f64,
//             (nn.calc(&[i as f64 / 10.])[0] * 10.).round()
//         );
//     });
// }

//  odd even
// fn main() {
//     /*
//         Define neural network with 1 neuron in input layers. Network contains 4 hidden layers.
//         And, such as our function returns single value, it is reasonable TO have 1 neuron in the output layer.
//     */
//     let mut nn = FeedForward::new(&[1, 8, 8, 8, 8, 1]);
//     /*
//         Define DataSet.
//         DataSet is the Type that significantly simplifies work with neural network.
//         Majority of its functionality is still under development :(
//     */
//     let training_data = {
//         (0..10).fold(DataSet::new(), |mut acc, i| {
//             acc.push(&[i as f64], &[(i % 2) as f64]);
//             acc
//         })
//     };
//     // Here, we set necessary parameters and train neural network by our DataSet with 50 000 iterations
//     nn.activation(Tanh)
//         .learning_rate(0.001)
//         .train(&training_data, 5_000_000);

//     (0..10).for_each(|i| {
//         println!("for [{:?}] -> [{:?}]", i, nn.calc(&[i as f64])[0].round());
//     });
// }

// fn main() {
//     /*
//         Define neural network with 1 neuron in input layers. Network contains 4 hidden layers.
//         And, such as our function returns single value, it is reasonable TO have 1 neuron in the output layer.
//     */
//     let mut nn = FeedForward::new(&[2, 8, 8, 8, 8, 1]);
//     /*
//         Define DataSet.
//         DataSet is the Type that significantly simplifies work with neural network.
//         Majority of its functionality is still under development :(
//     */
//     let training_data = {
//         let mut ds = DataSet::new();
//         ds.push(&[0., 0.], &[1.0]);
//         ds.push(&[0., 1.], &[0.0]);
//         ds.push(&[1., 0.], &[1.0]);
//         ds.push(&[1., 1.], &[0.0]);
//         ds
//     };
//     // Here, we set necessary parameters and train neural network by our DataSet with 50 000 iterations
//     nn.activation(Tanh)
//         .learning_rate(0.01)
//         .train(&training_data, 5_000);

//     let mut check = |input| println!("for [{:?}] -> [{:?}]", input, nn.calc(input));
//     check(&[0., 0.]);
//     check(&[1., 1.]);
//     check(&[1., 0.]);
//     check(&[1., 1.]);
// }

//  IDENTITY 3
// fn main() {
//     /*
//         Define neural network with 1 neuron in input layers. Network contains 4 hidden layers.
//         And, such as our function returns single value, it is reasonable TO have 1 neuron in the output layer.
//     */
//     let mut nn = FeedForward::new(&[2, 8, 8, 8, 8, 2]);
//     /*
//         Define DataSet.
//         DataSet is the Type that significantly simplifies work with neural network.
//         Majority of its functionality is still under development :(
//     */
//     let training_data = {
//         (1..100).fold(DataSet::new(), |mut acc, i| {
//             let d2 = i - (i / 10) * 10;
//             let d1 = (i - d2) / 10;
//             acc.push(
//                 &[d1 as f64, d2 as f64],
//                 &[d1 as f64 / 10.0, d2 as f64 / 10.0],
//             );
//             acc
//         })
//     };
//     // Here, we set necessary parameters and train neural network by our DataSet with 50 000 iterations
//     nn.activation(Tanh)
//         .learning_rate(0.01)
//         .train(&training_data, 50_000);

//     (0..100).for_each(|i| {
//         let d2 = i - (i / 10) * 10;
//         let d1 = (i - d2) / 10;
//         let calc = nn.calc(&[d1 as f64, d2 as f64]);
//         let r = ((calc[0] * 10.).round() * 10.) as i32 + (calc[1] * 10.0).round() as i32;
//         println!(
//             "for [{:.3}], [{:?}] -> [{:?}]  [{:?}]",
//             i,
//             &[d1, d2],
//             calc,
//             r
//         );
//     });
// }

//  IDENTITY 1
// fn main() {
//     /*
//         Define neural network with 1 neuron in input layers. Network contains 4 hidden layers.
//         And, such as our function returns single value, it is reasonable TO have 1 neuron in the output layer.
//     */
//     let mut nn = FeedForward::new(&[1, 8, 8, 8, 8, 1]);
//     /*
//         Define DataSet.
//         DataSet is the Type that significantly simplifies work with neural network.
//         Majority of its functionality is still under development :(
//     */
//     let training_data = {
//         (1..100).fold(DataSet::new(), |mut acc, i| {
//             acc.push(&[i as f64], &[i as f64 / 100.]);
//             acc
//         })
//     };
//     // Here, we set necessary parameters and train neural network by our DataSet with 50 000 iterations
//     nn.activation(Tanh)
//         .learning_rate(0.01)
//         .train(&training_data, 1_000_000);

//     (0..100).for_each(|i| {
//         let calc = nn.calc(&[i as f64])[0];
//         println!("for [{:.3}] -> [{:.10}]", i, calc * 100.);
//     });
// }

// fn main() {
//     /*
//         Define neural network with 1 neuron in input layers. Network contains 4 hidden layers.
//         And, such as our function returns single value, it is reasonable TO have 1 neuron in the output layer.
//     */
//     let mut nn = FeedForward::new(&[1, 8, 8, 8, 8, 1]);
//     /*
//         Define DataSet.
//         DataSet is the Type that significantly simplifies work with neural network.
//         Majority of its functionality is still under development :(
//     */
//     let mut data: DataSet = DataSet::new();
//     let mut i = -3.0;
//     // Push the data TO DataSet (method push accepts two slices: input data and expected output)
//     while i <= 2.5 {
//         data.push(&[i], &[0.5 * (i.exp().sin()) - (-i.exp()).cos()]);
//         i += 0.05;
//     }
//     // Here, we set necessary parameters and train neural network by our DataSet with 50 000 iterations
//     nn.activation(Tanh).learning_rate(0.01).train(&data, 50_000);

//     let mut res;
//     // Let's check the result
//     i = 0.0;
//     while i <= 0.3 {
//         res = nn.calc(&[i])[0];
//         println!(
//             "for [{:.3}], [{:.3}] -> [{:.3}]",
//             i,
//             0.5 * (i.exp().sin()) - (-i.exp()).cos(),
//             res
//         );
//         i += 0.07;
//     }
// }
