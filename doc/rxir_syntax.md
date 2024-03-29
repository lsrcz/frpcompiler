# Syntax of the RxIR

The RxIR is translated from the IR, and it models the low-level functionalities as the RxJS.

## Related files

- [rxir/inst.rkt](../rxir/inst.rkt)

## Streams

### `name-ref`

`(struct rx-name-ref (name) #:transparent)`

This is for input instruction, the name should be an input stream.

### `stream-ref`

`(struct rx-stream-ref (stream) #:transparent)`

This refers to a stream (input instruction/other instruction).

### `stream-pair-ref`

`(struct rx-stream-pair-ref (stream num) #:transparent)`

This refers to a stream in a stream pair. The stream pair is generated by `partition` instruction.

### `merge`

`(struct rx-merge (stream-list) #:transparent)`

This is RxJS' merge operator.

### `merge-action`

`(struct rx-merge-action (stream-list) #:transparent)`

This is RxJS's merge operator, then the result would be collected by iteratively applied to the accumulator.

### `pipe`

`(struct rx-pipe (stream ops) #:transparent)`

- `stream`: The source of the pipeline.
- `ops`: The list of the operators.

This refers to a stream which is the source stream transformed by the operator pipeline.

### `custom`

`(struct rx-custom (stream name) #:transparent)`

This refers to a stream which is the source stream transformed by the function specified by `name`.

### `empty`

`(struct rx-empty () #:transparent)`

This refers to a stream that never emits and never completes.

### `action`

`(struct rx-action (action-source) #:transparent)`

This refers a stream constructed by collecting the actions from the `action-source`.

## Operators

### `start-with-undefined`

`(struct rx-start-with-undefined () #:transparent)`

This is `startWith(undefined)` in RxJS.

### `buffer-count`

`(struct rx-buffer-count (buffer-size start-buffer-every) #:transparent)`

This is `bufferCount(buffer-size, start-buffer-every)` in RxJS.

### `map-shape`

`(struct rx-map-shape (from-shape to-shape) #:transparent)`

Example:

`(rx-map-shape '(a b c) '(a b (f a)))` would be `map(([a, b, c]) => [a, b, f(a)])`.

`(rx-map-shape 'a '(a (f a)))` would be `map((a) => [a, f(a)])`.

### `with-latest-from`

`(struct rx-with-latest-from (streams) #:transparent)`

This is `withLatestFrom(...streams)` in RxJS.

### `map-flat`

`(struct rx-map-flat (from-nested-shape to-shape) #:transparent)`

Example:

`(rx-map-shape '(a (b c)) '(a b c)` would be `map(([a, [b, c]]) => [a, b, c])`.

### `filter`

`(struct rx-filter (from-shape arg) #:transparent)`

Example:

`(rx-filter '(a b c) 'a)` would be `filter(([a, b, c]) => a)`.

### `partition`

`(struct rx-partition (from-shape arg) #:transparent)`

Example:

`(rx-partition '(a b c) 'a)` would be `partition(([a, b, c]) => a)`.

### `ret-action`

`(struct rx-ret-action (from-shape return-val action) #:transparent)`

Example

```racket
(rx-ret-action '(a b c) 'ret '(if-else ret (return a) (return b)))
```

would be

```javascript
map(([a b c]) => (ret) => {
    if (ret) {
        return a;
    } else {
        return b;
    }
})
```

### `switch-map`

`(struct rx-switch-map (from-shape body) #:transparent)`

Example:

```racket
(define some-rxir-list ...)
(rx-switch-map '(a b) (list 'if 'a some-rxir-list))
```

would be

```javascript
switchMap(([a, b]) => {
    if (a) {
        // translated some-rxir-list
    }
})
```

### `to-action`

`(struct rx-to-action (return-val) #:transparent)`

This is for wrapping the values into an action.

`(rx-to-action 'a)` would be `map((e) => (a) => e)`.

### `scan-undefined`

`(struct rx-scan-undefined (from-shape return-val action) #:transparent)`

This is the scan operator in RxJS, the initial value is set to `undefined`.

## RxJS instruction list

`(struct rxir-input-inst (name num inst) #:transparent)`
`(struct rxir-list (input-insts lst) #:transparent)`

The `rxir-input-inst` represents an RxIR input stream. The `rxir-list` represents the list of the input streams, and other streams translated from the IR.

