[![](https://github.com/doomdavve/maldives/workflows/ci/badge.svg)](https://github.com/doomdavve/maldives/actions)

# maldives
Toy interpreter for a toy language

```
> let a = 10
TypedExpression { resolved_type: Integer, node: Integer(10) }
> { let a = 20; a }
TypedExpression { resolved_type: Integer, node: Integer(20) }
> a
TypedExpression { resolved_type: Integer, node: Integer(10) }
> let c = { let a = 20; fn b() -> int = a; b }
TypedExpression { resolved_type: Function(ResolvedFunctionType { return_type: Integer, parameters: [] }), node: Function(TypedFunctionExpr { sym: Some("b"), parameters: [], expr: TypedExpression { resolved_type: Integer, node: Symbol("a") } }) }
> c()
TypedExpression { resolved_type: Integer, node: Integer(20) }
> a
TypedExpression { resolved_type: Integer, node: Integer(10) }
```

The sample above shows off lexical binding and closures.

#### Near term TODO

 - [X] Lexical scoping
 - [X] Strings
 - [X] Factor out Expression from parser
 - [X] Write/print to somewhere (println)
 - [X] Add types to expressions
 - [X] Add equal operator
 - [ ] Operator precedence
 - [ ] Pretty print expressions
 - [ ] Add Loops
 - [ ] Add Arrays
 - [ ] Add structures
