## TODO
* Detect left recursion


## Recursion

### Rigth recursion

No problem with descendent recursive parsers

```
ar  =   "a" ar
    /   "a"

ar  =   "a" ar?
```

### Left recursion

Infinite loop with descendent recursive parsers

```
al  =   al  "."
    /   "a"

al  =   al?  "a"  "."

```

Not so evident...

```
al  =   ("a" / al?) "."


al  =   al*  "a"  "."
```