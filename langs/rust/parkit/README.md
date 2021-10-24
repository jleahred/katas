# Parkit

I't an experimental parser toolkit

* LL(n)
* Caché
* Depth 1 left recursive

## TODO

  * Error display
  * Error context info per expresion or per rule
  * Error if not full parsing
  * cache
  * Error control
  * External tree creation
  * Repeat, ranges...
  * ...



## About recursion

### Rigth recursion

No problem with descendent recursive parsers like LL(n)

```
ar  =   "a" ar
    /   "a"

ar  =   "a" ar?
```

### Left recursion

Infinite loop with descendent recursive parsers

```
al  =   al
    /   "a"
```

Or more concise...

```
al  =   al?  "a"
```

Not so evident...

```
al  =   ("a" / al?) "."
```



First, we need to detect the left recursion

After that, there are some algorithms to manage left recursion on LL parsers, but...
code is not very elegant

Why do we need left recursion?

Going back to the simple example of left recursion...

```
al  =   al  "a"
    /   "a"
```

This will accept...

    a
    aa
    aaa
    aaaaaa

But is same and trivial with right recursion

```
ar  =   "a"  ar
    /   "a"
```


> note
> I know in this example is not neceesary recursion. It coul be solve
> with just...
> ```
>       as  =  "a"+
> ```


Rigth recursion and left recursion derivatives by oposite sides.
Some times you will need right or left asociativity, therefore...

This is one of the most common reason to support both.

But... I'm not sure.

IMHO the first step of a parser, has to check the correctness of the input and produce nice error messages.

After that, we can work with a parser tree, and take attention to associativity, priority...

It's common in LL parsers, to force the parser to create an AST with correct priority an associativity.

Not sure it's a good idea. In my experience, this ofuscate the parser, and the error messages.

### To left or not to left

Then... Can we remove right recursion, as we are not interestes on associativity and priorirty?

No so fast. I'm interested on clear grammars and good error messages

Let's write a PEG grammar for arithmetic expressions

No left recursion
```
    expr        =   number  ( op  expr )?
                /   "("   expr   ")"

    number      =   [0-9]+
```

With left recursion
```
    expr        =   expr  (op   expr)?
                /   "("   expr   ")"
                /   number

    number      =   [0-9]+
```

Looks very similar. Even the no left recursion is smaller

But lets add variables

No left recursion
```
    expr        =   subexpr  ( op  expr )?
                /   "("   expr   ")"
    subexpr     =   number  /  variable

    number      =   [0-9]+
    variable    =   [a-zA-Z_][a-zA-Z_0-9]*
```

With left recursion
```
    expr        =   expr  (op   expr)?
                /   "("   expr   ")"
                /   number
                /   variable

    number      =   [0-9]+
    variable    =   [a-zA-Z_][a-zA-Z_0-9]*
```

Let's add functions...

No left recursion
```
    expr        =   subexpr  ( op  expr )?
                /   "("   expr   ")"
    subexpr     =   number  /  function  /  variable

    number      =   [0-9]+
    variable    =   id
    function    =   id ( expr )
    id          =   [a-zA-Z_][a-zA-Z_0-9]*
```

With left recursion
```
    expr        =   expr  (op   expr)?
                /   "("   expr   ")"
                /   number
                /   variable
                /   function

    number      =   [0-9]+
    function    =   id ( expr )
    variable    =   id
    id          =   [a-zA-Z_][a-zA-Z_0-9]*
```

Not a big deal. A bit more clear with 1 depth left recursion

This is what I'm going to test with this library
