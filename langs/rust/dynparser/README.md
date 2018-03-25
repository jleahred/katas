# DynParser

A small and simple Dynamic Parser

## Usage

Add to `cargo.toml`

```toml
[dependencies]
pending...
```

See examples below

## Modifications

0.1.0 First version

## TODO/DONE

### TODO

* mostly all

### DONE

* --

## Input

### Grammar

#### Rule elements enumeration

Examples below

| token    | Description                                            |
| -------- | ------------------------------------------------------ |
| `=`      | On left, symbol, on right expresion defining symbol    |
| `symbol` | On right, it's an string without quotes                |
| `.`      | Any char                                               |
| `"..."`  | Literal delimited by quotes                            |
| `space`  | Separate tokens and Rule concatenation (and operation) |
| `/`      | Or operation                                           |
| `(...)`  | A expression composed of sub expresions                |
| `?`      | One optional                                           |
| `*`      | Repeat 0 or more                                       |
| `+`      | Repeat 1 or more                                       |
| `!`      | negate expression                                      |
| `[...]`  | Match chars. It's a list or ranges (or both)           |
| `->`     | pending...                                             |
| `:`      | pending...                                             |

Let's see by example

A simple literal string.
