# Preprocessing of the specification

## Related files
- [ast/monad-desugar.rkt](../ast/monad-desugar.rkt)
- [ast/expand.rkt](../ast/expand.rkt)

To simplify analyzing, we need some simple preprocessing of the specification.

## Monad desugar
Since we are modeling monadic programming and we introduced `begin` notation, which is analogous to `do` notation in haskell, we need to desugar it to `bind` notation.

Example:

```racket
(begin
  (let a (f input1))
  (let b (g a input2))
  (return (h a b)))
```

would be desugared to

```racket
(bind a (f input1)
  (bind b (g a input2)
    (return (h a b))))
```

## JavaScript expression extract
The JavaScript expressions inside the non-bind instructions are extracted as temporary values introduced with `bind` operator.

Example:

```racket
(if (f a) (return (g b)))
```

would be extracted to

```racket
(bind _temp1 (f a)
  (if _temp1
    (bind _temp0 (g b)
      (return _temp0))))
```