[![](https://github.com/doomdavve/maldives/workflows/ci/badge.svg)](https://github.com/doomdavve/maldives/actions)

# maldives
Toy interpreter for a toy language

```
> let a = 10
Integer(10)
> { let a = 20; a }
Integer(20)
> a
Integer(10)
> let c = { let a = 20; fn b() -> int = a; b }
Function(FunctionExpr { sym: Some("b"), return_type: Symbol("int"), parameters: [], expr: Symbol("a") })
> c()
Integer(20)
> a
Integer(10)
```

The sample run above shows off lexical binding and closures.

#### Near term TODO

 - [X] Lexical scoping
 - [X] Strings
 - [X] Factor out Expression from parser
 - [X] Write/print to somewhere (println)
 - [X] Add types to expressions
 - [ ] Add equal operator
 - [ ] Pretty print expressions
 - [ ] Add Loops
 - [ ] Add Arrays
 - [ ] Add structures
