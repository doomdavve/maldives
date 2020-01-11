[![](https://github.com/doomdavve/maldives/workflows/ci/badge.svg)](https://github.com/doomdavve/maldives/actions)

# maldives
Toy interpreter for a toy language

```
>  let a = 10
10: int
> { let a = 20; a }
20: int
> a
10: int
> let c = { let a = 20; fn b(x: int, y: int) -> int = x + y + a; b }
fn b(): (int, int) -> int
> c()
Type resolve error: Type mismatch: argument mismatch
> c(10, 20)
50: int
> a
10: int
```

The sample above shows off type resolver, crude error messages,
expression blocks, lexical binding and closures.

#### Near term TODO

 - [X] Lexical scoping
 - [X] Strings
 - [X] Factor out Expression from parser
 - [X] Write/print to somewhere (println)
 - [X] Add types to expressions
 - [X] Add equal operator
 - [X] Pretty print expressions
 - [ ] Operator precedence
 - [ ] Add Loops
 - [ ] Add Arrays
 - [ ] Add structures
