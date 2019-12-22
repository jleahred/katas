extern crate neuroflow;

use neuroflow::activators::Type::Tanh;
// use neuroflow::activators::Type::Sigmoid;
use neuroflow::data::DataSet;
use neuroflow::FeedForward;

fn main() {
    /*
        Define neural network with 1 neuron in input layers. Network contains 4 hidden layers.
        And, such as our function returns single value, it is reasonable to have 1 neuron in the output layer.
    */
    // let mut nn = FeedForward::new(&[1, 8, 8, 8, 8, 1]);
    let mut nn = FeedForward::new(&[1, 2, 1]);
    /*
        Define DataSet.
        DataSet is the Type that significantly simplifies work with neural network.
        Majority of its functionality is still under development :(
    */
    let training_data = {
        (0..10).fold(DataSet::new(), |mut acc, i| {
            if i != 5 {
                acc.push(&[i as f64], &[(i * 2) as f64 / 10.0]);
            }
            acc
        })
    };
    // Here, we set necessary parameters and train neural network by our DataSet with 50 000 iterations
    nn.activation(Tanh)
        .learning_rate(0.001)
        .train(&training_data, 100_000);

    (0..10).for_each(|i| {
        println!(
            "for [{:?}] -> [{:?}]",
            i as f64,
            (nn.calc(&[i as f64])[0] * 10.).round()
        );
    });
}

//  odd even
// fn main() {
//     /*
//         Define neural network with 1 neuron in input layers. Network contains 4 hidden layers.
//         And, such as our function returns single value, it is reasonable to have 1 neuron in the output layer.
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
//         And, such as our function returns single value, it is reasonable to have 1 neuron in the output layer.
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
//         And, such as our function returns single value, it is reasonable to have 1 neuron in the output layer.
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
//         And, such as our function returns single value, it is reasonable to have 1 neuron in the output layer.
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
//         And, such as our function returns single value, it is reasonable to have 1 neuron in the output layer.
//     */
//     let mut nn = FeedForward::new(&[1, 8, 8, 8, 8, 1]);
//     /*
//         Define DataSet.
//         DataSet is the Type that significantly simplifies work with neural network.
//         Majority of its functionality is still under development :(
//     */
//     let mut data: DataSet = DataSet::new();
//     let mut i = -3.0;
//     // Push the data to DataSet (method push accepts two slices: input data and expected output)
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
