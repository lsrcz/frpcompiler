# Syntax of the specification

## Related files

- [test/test-spec.rkt](../test/test-spec.rkt)
- [ast/syntax.rkt](../ast/syntax.rkt)

## Definition of the specification

```racket
(struct spec (inputs output funclist constantlist body) #:transparent)
```
members:

- `inputs`: A list of symbols, representing the name of the input streams.
- `output`: A symbol, representing the name of the output stream.
- `funclist`: A list of symbols, representing the abstract functions that can be used in the specification.
- `constantlist`: A list of symbols, representing the constant values that can be used in the specification.
- `body`: A list of transition functions. A transition function is a list, the first item is a symbol, representing the start point of the pipeline, and the second item is the body of the function.

## Syntax of the specification body

The specification body is generated by the following grammar:

```text
spec-body        => "(" trans-func-list ")"
trans-func-list  => trans-func
                 |  trans-func trans-func-list
trans-func       => "(" symbol inst ")"

inst             => "(" inst-no-brackets ")"
inst-no-brackets => if-inst
                 |  if-else-inst
                 |  begin-inst
                 |  return-inst
                 |  split-inst
                 |  custom-inst
if-inst          => "if" js-expr inst
if-else-inst     => "if-else" js-expr inst inst
return-inst      => "return" js-expr
custom-inst      => "custom" symbol inst

begin-inst       => "begin" let-inst-list inst
                 |  "begin" inst
let-inst         => "(" "let" symbol js-expr ")"
let-inst-list    => let-inst
                 |  let-inst let-inst-list

split-inst       => "split" "()" imp-inst
                 |  "split" "(" binding-list ")" imp-inst
binding-list     => binding
                 |  binding binding-list
binding          => "(" symbol js-expr ")"

imp-inst         => "(" imp-inst-no-brackets ")"
imp-inst-no-brackets  => if-imp-inst
                      |  if-else-imp-inst
                      |  begin-imp-inst
                      |  new-stream-inst
                      |  empty-stream-inst
if-imp-inst      => "if" js-expr imp-inst
if-else-imp-inst => "if-else" js-expr imp-inst imp-inst

begin-imp-inst   => "begin" let-inst-list imp-inst
                 |  "begin" imp-inst

empty-stream-inst => "empty-stream"
new-stream-inst  => new-stream spec-body


```

where `js-expr` is an expression in JavaScript which can reference to the functions/bindings/input streams, and it's written in S-expression, and a `symbol` is any symbol.

## Interpretation of the syntax

### `js-expr`

A `js-expr` is an S-expression. `(f (g a))` means `f(g(a))` in JavaScript. In a `js-expr`, the symbols can refer to various things.
- Function: in the `funclist`.
- Constant: in the `constantlist`.
- Stream:
  1. The symbol can refer to the current stream, which is the source of the transition function, and the value would be always the event triggered the transition.
  2. The symbol can refer to other streams, and the value would be the newest value for the stream.
  3. Special syntax for refering to the previous values in the other streams: `(prev (.... (prev a)))` means the (n + 1)-th latest value of stream a, where n is the number of `prev`.

### monadic programming

The DSL formulates monadic programming as RxJS, so we can write the whole program as a pipe line.

### `if`

`(if value branch)` means

```javascript
if (value) {
    branch
}
```

in JavaScript.

### `if-else`

`(if-else value then_branch else_branch)` means

```javascript
if (value) {
    then_branch
} else {
    else_branch
}
```

in JavaScript.

### `return`

`(return value)` means

```javascript
return value;
```

in JavaScript.

### `begin`

`(begin (let name1 value1) (let name2 value2) ... inst)` means

```javascript
const name1 = value1;
const name2 = value2;

...

inst
```

### `custom`

The DSL allows the user to plugin his own stream operators/functions to the program via `custom` instruction.

`(custom func next-inst)` means applying the function `func` to the stream piped to the `custom` instruction, and then pipe the result to `next-inst`.

### `split`

The `split` operator captures the semantics of creating new streams and different transtion functions based on some input value. It would be translated to RxJS `switchMap` operator.

`(split ((name1 value1) (name2 value2)) imp-inst)` means binding the values to the names which can be used in the `imp-inst`, and we can create new streams in the `imp-inst`(see next).

### `imp-inst`

The `imp-inst` is the operator for imperative programming, which is used in the body of the `split` operator. The instructions are translated to equivalent statements in JavaScript.

### `empty-stream`

`empty-stream` means creating a new stream which has no value but also never completes. It's `NEVER` in RxJS.

### `new-stream`

`new-stream` means creating a new stream based on the transition functions in its body.


