# Syntax of the IR

This IR is translated directly from the spec, and it models some high-level functionalities.

## Related files

- [ir/inst.rkt](../ir/inst.rkt)

## Key definitions

- Terminal instruction: An instruction that could be the return value, no shape is maintained.
  - [ret](#ret)
  - [ret-action](#ret-action)
  - [merge](#merge)
  - [merge-action](#merge-action)
  - [split](#split)
  - [split-action](#split-action)
  - [action](#action)
  - [scan](#scan)
- Non-terminal instruction: An instruction that should be piped to another instruction, a shape is associated.
  - [input](#input)
  - [intro](#intro)
  - [intro-const](#intro-const)
  - [compute](#compute)
  - [filter](#filter)
  - [partition](#partition)
  - [custom](#custom)
- Shape: A list or a single symbol representing what's inside the stream.
- Action: An action is a function which accepts an argument representing the previous value of the result stream, and computes the next value for the result stream.

## Instructions

### `input`

#### Definition

`(struct input-inst (name num shape) #:transparent)`

- `name`: The name of the input stream.
- `num`: The maximum number of `prev` wrapping the `name` in the spec.

#### Semantics

The `input` instruction constructs a stream containing the current value and the previous values.

Example:

`(input-inst 'a 0 'a)` is just the stream `a`, while `(input-inst 'a 1 '(a (prev a)))` is a stream containing the value and the previous value of `a` at the time when `a` emits.

### `intro`

#### Definition

`(struct intro-inst (intro-lst ref shape) #:transparent)`

- `intro-lst`: The list of the streams that should be combined with the current stream, should all be `input` instructions.
- `ref`: The source of the pipeline.

#### Semantics

The `intro` instruction combines the streams specified in the `intro-lst` to the `ref` stream.

Example:

```racket
r1 = (input-inst 'a 0 'a)
r2 = (input-inst 'b 1 '(b (prev b)))
r3 = (intro-list (list r2) r1 '(a b (prev b)))
```

The semantics is shown in `r3`'s shape.

### `intro-const`

#### Definition

`(struct intro-const-inst (intro-lst ref shape) #:transparent)`

- `intro-lst`: The list of the constants that should be combined with the current stream.
- `ref`: The source of the pipeline.

#### Semantics

The `intro-const` instruction adds the constant values specified in the `intro-lst` to the `ref` stream.

Example:

```racket
r1 = (input-inst 'a 0 'a)
r2 = (intro-const-inst '(b) r1 '(a b))
```

The semantics is shown in `r2`'s shape.

### `compute`

#### Definition

`(struct compute-inst (to-compute name ref shape) #:transparent)`

- `to-compute`: The expression that should be computed based on the values in the `ref` stream.
- `name`: The name to bind with.
- `ref`: The source of the pipeline.

#### Semantics

The `compute` instruction computes the `to-compute` and bind it as `name` in the result stream.

Example:

```racket
r1 = (input-inst 'a 0 'a)
r2 = (compute-inst '(f a) 'p r1 '(a p))
```

The semantics is shown in `r2`'s shape, and `p` should always equal to `f(a)`.

### `filter`

#### Definition

`(struct filter-inst (arg ref shape) #:transparent)`

- `arg`: The value to filter with, should be a symbol that appears in the `ref` stream's shape.
- `ref`: The source of the pipeline.

#### Semantics

The `filter` instruction filters the `ref` stream based on the `arg`. It won't modify the shape of `ref`.

Example:

```racket
r1 = (input-inst 'a 0 'a)
r2 = (filter 'a r1 'a)
```

the marble diagram:

```text
r1 ---T---F----T---F--
r2 ---T--------T------
```

### `partition`

#### Definition

`(struct partition-inst (arg ref shape) #:transparent)`

- `arg`: The value to partition with, should be a symbol that appears in the `ref` stream's shape.
- `ref`: The source of the pipeline.

#### Semantics

The `partition` instruction partitions the `ref` stream based on the `arg`. It won't modify the shape of `ref`. The return value is an array with two items. The first item is `(filter arg ref shape)`, and the second item is `(filter not arg) ref shape)`

### `custom`

#### Definition

`(struct custom-inst (name ref shape) #:transparent)`

- `name`: The name of the external stream transform function.
- `ref`: The stream being transformed.

#### Semantics

The result stream is `name(ref)`.

### `ret`

#### Definition

`(struct ret-inst (arg ref) #:transparent)`

- `arg`: The name of the value that should be returned.
- `ref`: The source of the pipeline.

#### Semantics

The result is a stream containing only the value of `arg`.

### `merge`

#### Definition

`(struct merge-inst (to-merge) #:transparent)`

- `to-merge`: A list of the streams that would be merged.

#### Semantics

The result is a merged stream of the streams in `to-merge`.

### `ret-action`

#### Definition

`(struct ret-action-inst (return-val action ref) #:transparent)`

- `return-val`: The name of the output stream.
- `action`: The body of the action.
- `ref`: The source of the pipeline.

#### Semantics

The result is a stream containing the actions. The body of the action is specified in `action`, and the argument of the action is `return-val`.

### `merge-action`

#### Definition

`(struct merge-action-inst (to-merge) #:transparent)`

- `to-merge`: A list of the streams of actions that would be merged.

#### Semantics

This instruction firstly merges the streams in `to-merge`, then collects them by iteratively applying them to the `undefined` initial value.

### `action`

#### Definition

`(struct action-inst (action-source) #:transparent)`

- `action-source`: The stream of actions that should be transformed to a stream of values.

#### Semantics

This instruction collects the actions by iteratively applying them to the `undefined` initial value. 

### `scan`

#### Definition

`(struct scan-inst (return-val action ref) #:transparent)`

- `return-val`: The name of the output value.
- `action`: The body of the action.
- `ref`: The source of the pipeline.

#### Semantics

This instruction is just the `scan` operator in RxJS, it's a syntax sugar of

```racket
(action-inst (return-action-inst return-val action ref))
```

### `split`

#### Definition

`(struct split-inst (bindings body ref) #:transparent)`

- `bindings`: A list of bindings that should be available in body.
- `body`: Imperative program whose return value should be streams specified by an [`ir-list`](#ir-list).
- `ref`: The source of the pipeline.

#### Semantics

This instruction projects each source value to a stream which is merged in the output stream, emitting values only from the most recently projected Observable.

Example:

```racket
r1 = (input-inst 'a 0 'a)
r2 = (split-inst '((b a))
       (if-else b ir-list1 ir-list2))
```

When `r1` emits `true`, the result is specified by `ir-list1`, when `r1` emits `false`, the result is specified by `ir-list2`.

### `split-action`

#### Definition

`(struct split-action-inst (return-val bindings body ref) #:transparent)`

- `return-val`: The name of the output stream.
- `bindings`: A list of bindings that should be available in body.
- `body`: Imperative program whose return value should be streams specified by an [`ir-list`](#ir-list).
- `ref`: The source of the pipeline.

#### Semantics

This instruction works the same as `split`, but the result values are projected to actions consists of constant functions.

### `empty`

#### Definition

`(struct empty-inst () #:transparent)`

#### Semantics

A stream that never emits value and never completes.

## Instruction list

The instruction list is an struct containing all the instructions for a stream.

### Definition

`(struct ir-list (input-lst lst ref-table-lst) #:transparent)`

- `input-lst`: The input streams(should all be `input` instructions).
- `lst`: The instructions for the stream, the stream for the last instruction is the result.
- `ref-table-lst`: Available constants.


