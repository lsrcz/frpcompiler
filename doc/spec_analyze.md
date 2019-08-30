# Analysis of the specification

## Related files

- [ast/find-missing.rkt](../ast/find-missing.rkt)
- [ast/find-retval.rkt](../ast/find-retval.rkt)

## Find missing variables

A `js-expr` may refer to some constants, some functions and some input streams, and we need to introduce them to the stream before we are trying to do some calculation with them.

Usually the IR has a shape (see [ir_syntax.md](ir_syntax.md)) field representing the available values maintained in the stream after that instruction, and we can refer to them in the calculation. Also, we can refer to the functions in `funclist` directly.

We cannot refer to the constants and the values from the input streams directly, so we need to introduce them to the pipeline.

The `find-missing-var` and `find-missing-var-deep` functions are trying to extract the values that should be introduced.

### Analysis result

The result would be wrapped in a struct defined as

```racket
(struct all-found () #:transparent)
(struct not-found (intro intro-const) #:transparent)
```

- `all-found`: All values are bounded.
- `not-found`: Some values are unbounded.
  - `intro`: List of the names of unbounded values from a stream.
  - `intro-const`: List of the names of unbounded values from a constant. Actually we can refer to them directly in JavaScript, but we choose to treat the stream values and constant values in an unified way so they are also introduced to the stream.

### `find-missing-var`

The `(find-missing-var shape funclst return-val ref-list arg)` function works on a single `js-expr` passed as the `arg`.

#### Arguments

- `shape`: The shape of the previous instruction.
- `funclst`: Available abstract functions.
- `return-val`: The name of the result stream.
- `ref-list`: Available constants.
- `arg`: The `js-expr` to analyze.

#### Algorithm

The function would iterate through the `arg`, extract the symbols(the symbols wrapped in the `prev`s would also be extracted), and then try to resolve them in the available lists.

If the symbol is found in `shape` or `funclst`, or it's the same as the `return-val`, it would be ignored. Then, if the symbol is found in `ref-list`, it would be returned in the `intro-const` as an unbound constant, or it would be returned in the `intro` field.

### `find-missing-var-deep`

The `(find-missing-var-deep shape funclst return-val ref-list inst)` is like the `find-missing-var-deep` function, but works for an action (see [ir_syntax.md](ir_syntax.md), [ir_translate.md](ir_translate.md)).
