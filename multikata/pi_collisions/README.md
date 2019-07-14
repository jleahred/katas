# PI number and collisions

Summer kata...

Surely you remember the fundamental characteristics of `PI` that have been
known for a long time.

But it is a number that holds curious surprises. If you are not aware of
the latest discoveries in this regard, let me tell you.

Not long time ago it was known that if you multiply `PI` by the diameter
(or by twice the radius, which is the same) you have the perimeter
of the circumference.

And if you multiply it by the square radius, you have the area of the circle.

Impressive! `o_O`

But what you surely remember, is the following
(known since looong and studied in primary school, in addition to Sesame Street)

Suppose we have a wall, an object called `s` and another
called `B` arranged like this...

(`s` for small and `B` for BIG)

```no_lang
   |
   |
   |
   |
   |         s         B
   _________________________________
```

Let us also suppose that we have perfect elastic collisions with no friction.

Now we throw the object of mass `B` against the object of mass `s` (which is stopped).
It doesn't matter how fast we do it.

Object `B` will collide with `s`, then `s` will collide with the wall, then with `B` again,
then with the wall again, then with `B`...

The number of collisions of `s` will depend on the mass of `B` and the mass of `s`
(it will not depend on the speed of `B`).

As you know, if the mass of `B` is equal to the mass of `s`, i.e. both are worth 1,
the number of shocks is 3.

If the mass of `B` is 100 times that of `s`, the number of shocks is 31.

If the mass of `B` is 10000000000 times that of `s`, the number of shocks 314159

Can you check this weird thing of the universe makers, with a program?

In case you don't remember, in linear elastic collisions the equations are:

```no_lang
    // collision two objects
    //
    // v1f = (2*m2*v2i + (m1-m2)*v1i) / (m1+m2)
    // v2f = (2*m1*v1i + (m2-m1)*v2i) / (m1+m2)
```

Enjoy

<https://youtu.be/HEfHFsfGXjs>
