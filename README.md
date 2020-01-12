[![](https://github.com/doomdavve/maldives/workflows/ci/badge.svg)](https://github.com/doomdavve/maldives/actions)

# maldives

Toy interpreter for a toy language. The language happens to be
syntactically similar to Rust but it has a Typescript kind of feel to
it.

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

```
> 77 == 1 + 8**2 + 2 * 6
true: bool
```

Now with proper operator precedence. Nothing beats trying to implement
infix parsing, getting half-way there and *then* reading about
[Operator-precedence
parser](https://en.wikipedia.org/wiki/Operator-precedence_parser
"Wikipedia's page"). It's a beautiful algorithm.

#### Near term TODO

 - [X] Lexical scoping
 - [X] Strings
 - [X] Factor out Expression from parser
 - [X] Write/print to somewhere (println)
 - [X] Add types to expressions
 - [X] Add equal operator
 - [X] Pretty print expressions
 - [X] Operator precedence
 - [ ] Add Loops
 - [ ] Add Arrays
 - [ ] Add structures
