Simple kata in `Rust` to get the prime at some pos

Quite functional

Only one mut in a function of three lines

"Lazy lists" kind (iterators in `Rust`)

```rust
    let pos_prime = 1_000_000;

    let prime_at_pos = primes::iter().skip(pos_prime - 1).next().unwrap();
```

Another option

```rust
    let prime_at_pos = primes::iter().take(pos_prime).last().unwrap();
```

Example to take first 100 primes

```rust
    primes::iter().take(100).collect::<Vec<_>>()
```


About `.unwrap()`

In this case, it's not a simplication to reduce code

In both cases it will have a value, and it's commented in code with...

```rust
//  I know there is always one ;-)
```