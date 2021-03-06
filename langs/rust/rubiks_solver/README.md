# Prelude

## Aim

A non trivial problem to learn, practice and to known Rust


## NOT Aim

A great rubik's cube solver.

I'm worried about Rust in this kata, not in fantastic algorithms to solve
the rubik's cube

## Practical

This program will be practical to find good rubik's sequences for cubes
of any size (not to fully solve the cube from any position)


# Rubik cubes solver

This is a small program to solve and look for combinations on rubiks cubes.

It can be configured to different cubes sizes on ```config.rs```

Example of search configuration with no solution:
```
let init : cube::Sides = cube::create_from_strings(
                 ["0000",
                  "0000",
                  "0000",
                  "0000",
             "0001 0000 0000",
             "0002 0000 0000",
             "0003 0000 0000",
             "0004 0000 0000",
                  "0000",
                  "0000",
                  "0000",
                  "0000",

                  "0000",
                  "0000",
                  "0000",
                  "0000"]
    );

let end : cube::Sides = cube::create_from_strings(
                ["0000",
                 "0000",
                 "0000",
                 "0000",
            "0000 0000 0000",
            "0000 0000 0000",
            "0000 0000 0000",
            "0000 0000 0000",
                 "1234",
                 "0004",
                 "0000",
                 "0000",

                 "0000",
                 "0000",
                 "0000",
                 "0000"]
            );


let start = time::PreciseTime::now();
let result = tree::explore(&init, &end, 6);
let end = time::PreciseTime::now();

println!("{}", result);
println!("required time: {}", start.to(end));
println!("iterations/sec: {}", result.iterations as f32 / start.to(end).num_milliseconds() as f32*1000.0);
```

Example of interesting parity problem on 4x4x4 cube:
```
let init : cube::Sides = cube::create_from_strings(
                 ["1111",
                  "1111",
                  "1111",
                  "1331",
             "2222 3113 4444",
             "2222 3333 4444",
             "2222 3333 4444",
             "2222 3333 4444",
                  "5555",
                  "5555",
                  "5555",
                  "5555",

                  "6666",
                  "6666",
                  "6666",
                  "6666"]
    );

let end : cube::Sides = cube::create_from_strings(
                ["1111",
                 "1111",
                 "1111",
                 "1111",
            "2222 3333 4444",
            "2222 3333 4444",
            "2222 3333 4444",
            "2222 3333 4444",
                 "5555",
                 "5555",
                 "5555",
                 "5555",

                 "6666",
                 "6666",
                 "6666",
                 "6666"]
            );
```

On end, the '0' sticker means it position doesn't matter


## Compilation

As you could imagine...

    cargo build

    cargo build --release

    cargo test

    cargo doc  (soon)



## Dessign

* ```cube::Sides``` contains  ```side::Stickers```

* ```side::Stickers``` is ```[[u8; SIZE]; SIZE]```

* ```cube::rot.rs``` has the rotation logic

* ```tree``` has the tree explorer code

* ```tree/opts``` has the optimizations applied on tree search

### Rotations

On ```cube::rot```

```
    pub enum Orient {
        Horizontal,
        Vertical,
        Front
    }
```

```
    pub enum Dir { Plus, Minus }
```

```
    pub struct Item(pub Orient, pub Dir, pub usize);
```

Output format examples:

    F+0 -> Front move, clock direction, level 0 (front side)
    H-2 -> Horizontal move, left direction, level2 (second row)
    V+3 -> Vertical move, right direction, level 3 (third col)





## Theory

Obviously, the brutal force search has an exponential cost ```O^n```

Calculating the required movements for a specific configuration, will let us
verify the search algorithm is not incorrect. I mean, we can verify there are
errors if the number is not the calculated one, but if the number is right
we cannot be 100% sure it is correct.

bcf means brutal force combinations...

    bfc(depth) = moves

    bfc(1) = 24
    bfc(2) = 24 + 24^2
    bfc(3) = bfc(3-1) + 24^3
    ...
    bfc(n) = bfc(n-1) + 24^n

    bfc(n) = bfc(n-2) + 24^(n-1) + 24^n
    bfc(n) = bfc(n-3) + 24^(n-2) + 24^(n-1) + 24^(n-0)

    bfc(n) = 24(1+24*(1+24*(1+24*...)))

Maximun number of moves required on brute-force search for a 4x4x4 cube

    depth: 1 -> moves: 24
    depth: 2 -> moves: 600
    depth: 3 -> moves: 14424
    depth: 4 -> moves: 346200
    depth: 5 -> moves: 8308824
    depth: 6 -> moves: 199411800
    depth: 7 -> moves: 4785883224
    depth: 8 -> moves: 114861197400
    depth: 9 -> moves: 2756668737624
    depth: 10 -> moves: 66160049703000
    depth: 11 -> moves: 1587841192872024
    depth: 12 -> moves: 38108188628928600
    depth: 13 -> moves: 914596527094286406
    depth: 14 -> moves: 21950316650262872444
    depth: 15 -> moves: 526807599606309062426
    depth: 16 -> moves: 12643382390551416022840

In order to verify the search algorithm, we have to disable the search
optimizations, configure an impossible problem and check the required iterations.


## Brute force

No optimizations, exploring all moves

| Depth        | moves           | debug(s) | release(s)  | move/sec
| ------------- |:-------------| -----: | -----: | -----:
| 1      | 24 | | | |
| 2      | 600 | 0.0002 | 0.0001 | |
| 3      | 14.424 | 0.12 | 0.003 | 4.808.000
| 4      | 346.200 | 2.10 | 0.074 | 4.678.378
| 5      | 8.308.824 | 49.6 | 1.06 | 7.868.204 |
| 6      | 199.411.800 |  | 31.6 | 6.304.116 |  

The difference between debug and release is huge.

This table let us predict the time with brute force search.

Estimations brute force:

    6 moves -> 30 s
    7 moves -> 12 min
    8 moves -> 5 h
    9 moves ->  5 days
    10 moves -> 4 months
    11 moves -> 8 years
    12 moves -> 200 years
    13 moves -> 4.800 years
    14 moves -> 116.000 years
    15 moves -> 2.8 million years
    16 moves -> 67 million years
    17 moves -> 1.600 mill years
    18 moves -> 38.488 mill years (too much time)
    19 ...


## Adding

### Save working try

Checking performance with depth 4

| Strategy        | moves/sec  | perf |
| -------------   |-----------:|-----:|
| No save copy      | 164621 | 104 |
| No save clone      | 158000 | 100 |
| Sharing      | 150260 | 95 |
| Full copy         | 125799 | 79 |

The clone path full copy solution is quite simple and elegant, but it has a logical
performance cost compared with Sharing and manual path stack managing

Working with Clone...

```
#[derive(Debug, Clone)]
pub struct Status {
    pub depth           : u8,
    pub max_depth       : u8,
    pub iterations      : u64,

    pub best_found      : Option<Found>,

    current_path_ref    : LinkedList<cube::rot::Item>,  //  <<----
}

impl Status {
    fn next_iteration(&self, rot: &cube::rot::Item) -> Status {
        let mut result = self.clone();
        result.iterations += 1;
        result.depth += 1;
        result.current_path_ref.push_back(*rot);    //  <<----
        result
    }

    fn new_update(&self, best_found : &Option<Found>,  iterations : u64) -> Status {
        let mut result = self.clone();
        result.best_found = *best_found;
        result.iterations += iterations;
        result
    }

```


Working with sharing and manage manually...

```
#[derive(Debug, Clone)]
pub struct Status {
    pub depth           : u8,
    pub max_depth       : u8,
    pub iterations      : u64,

    pub best_found      : Option<Found>,

    current_path_ref    : Rc<RefCell<LinkedList<cube::rot::Item>>>,  <<------
}

impl Status {
    fn next_iteration(&self, rot: &cube::rot::Item) -> Status {
        let mut result = self.clone();
        result.iterations += 1;
        result.depth += 1;
        result.current_path_ref.borrow_mut().push_back(*rot);   //  <<----
        result
    }

    fn new_update(&self, best_found : &Option<Found>,  iterations : u64) -> Status {
        let mut result = self.clone();
        result.best_found = *best_found;
        result.iterations += iterations;
        result.current_path_ref.borrow_mut().pop_back();  //  <<----
        result
    }
```

In this case we could try to use borrowed pointers managing the scope manually.

We could also try with Boxing, very close to raw pointer performance.

But I will use Rc pointers because the solution is simpler and the performance
is good enougth.

> .clone() on Rc will produce two pointers at same object, but .clone()
on Box, will will copy the value and we will have two pointers.

And remember, the compiler will remove the counter when it can detect that
it's not necessary (same as Swift).

### Save path finally

Letting the language to manage the stack is elegant, but not always good
for performance.

In this case, there are some fields on status that has to be updated and
saved on backward direction.

One option, is to configure these fields as ```Rc<RefCell<```.

The other option, better on performance, is to manage the status manually.

Making the push and pop and modifying the values when entering and leaving
a branch




## Optimizations (opt)

1. Depth will be adjusted as a solution is found
- Tree punning with different strategies
    1. Avoid repeating a move tree times
    - Avoid repeat position on current path (checking from end to begin)
    - If a movement is at same orientation that previous one, it has to bee on a higher level
- Back to front positions generation in memory (last moves caché).
  This will let us to increase depth search in some steps (estimation 4-6) using calculations in memory

### Last moves cache

This is a great optimization.

Starting by end, the program will run with depth 5 memorizing the positions.

When running the tree, on each movement, it will look in this cache.

This let to add 5 in tree search depth.

Therefore, we can run 12 or 13 with no problems at all.

Remember, 12 cost is 200 years, and 13 cost is thousands of years



## Conclusions

This exercise has been great to practice and test Rust.

I've used borrowing, Box, Rc, modules, cargo, unsafe...

Now, I also know some WTF about this language and the future about them.
I will write some ideas about it soon.

This program is practical to find sequences directly or indirectly.
You could look for symmetrical sequences (very common), doubling the potential
depth of the program.



## TODO

* Code documentation
