# Machine check work distribution

## The problem

We have _n_ machines

Each one, has to be checked with different procedures

Each procedure/machine require different time

We need to distribute all the checks (machine/period/kind)

A distribution _A_ is better than another _B_ if _A_ requires more similar times
per period than _B_

Possible configuration

    // machine  A	B	C	time A	    time B	    time C
    // 1        8	2	2	15	        20	        30
    // 2	    7	3	2	20	        30	        50
    // 3	    6	4	2	25	        35	        55
    // 4	    8	2	2	15	        20	        30
    // 5	    8	2	2	12	        15	        28
    // 6	    8	2	2	15	        20	        30
    // 7	    7	3	2	20	        30	        50
    // 8	    6	4	2	25	        35	        55
    // 9	    8	2	2	15	        20	        30
    // 10	    8	2	2	12	        15	        28
    // 11	    8  	2	2	15	        20	        30
    // 12	    8	2	2	12	        15	        28


All machines requires 12 checks, therefore we will distribute on 12 periods

You can insert a new check type like _Z_ with cost 0 if different quantity of checks
are required

Machines can have different types and quantity of _check kinds_. In this example,
for simplicity we will use _A, B, C_ for all machines

```rust
    let machines = machine_list!{
        //  machine_code, [ (num_visits, minut_visits), (num_visits, minut_visits), ...]
        { "m1",  [( 8, 15), ( 2, 20), ( 2, 30)] },
        { "m2",  [( 7, 20), ( 3, 30), ( 2, 50)] },
        { "m3",  [( 6, 25), ( 4, 35), ( 2, 55)] },
        { "m4",  [( 8, 15), ( 2, 20), ( 2, 30)] },
        { "m5",  [( 8, 12), ( 2, 15), ( 2, 28)] },
        { "m6",  [( 8, 15), ( 2, 20), ( 2, 30)] },
        { "m7",  [( 7, 20), ( 3, 30), ( 2, 50)] },
        { "m8",  [( 6, 25), ( 4, 35), ( 2, 55)] },
        { "m9",  [( 8, 15), ( 2, 20), ( 2, 30)] },
        { "m10", [( 8, 12), ( 2, 15), ( 2, 28)] },
        { "m11", [( 8, 15), ( 2, 20), ( 2, 30)] },
        { "m11", [( 8, 12), ( 2, 15), ( 2, 28)] }
    };
```