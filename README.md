# RxJS Compiler

The compiler compiles RxJS program from transition-style specification.

## Usage

```racket
#lang racket

(require "compile.rkt")
(require "print/rxir.rkt")

(print-rx-program (compile some-spec))

```

## Example

The specification

```racket
(spec
 '(mode down move)
 'drawing
 '(f g l n)
 '()
 '((mode
    (split ((mode-snapshot mode)
            (down-snapshot down))
           (if-else (f mode-snapshot)
                    (new-stream
                     ((move (if-else (g drawing)
                                     (return (l down-snapshot move))
                                     (return (n drawing (prev move)
                                     move))))))
                    (empty-stream))))))
```

would be compiled to

```javascript
function compiled(mode, down, move) {
  const move1 = move.pipe(
    startWith(undefined),
    bufferCount(2, 1),
  );
  const r0 = mode.pipe(
    withLatestFrom(down),
  );
  return r0.pipe(
    switchMap(([mode-snapshot, down-snapshot]) => {
      const _temp0 = f(mode-snapshot);
      if (_temp0) {
        return move1.pipe(
          scan((drawing, [move, prev_move]) => {
            const _temp2 = g(drawing);
            if (_temp2) {
              return l(down-snapshot, move);
            } else {
              return n(drawing, prev_move, move);
            }
          }, undefined),
          filter(Boolean),
        );
      } else {
        return NEVER;
      }
    }),
  );
}
```

The specification

```racket
(spec
 '(mode move down)
 'drawing
 '(f h g m l n)
 '()
 (list
  '(mode
    (if-else (f mode)
        (return (h))
        (return (g))))
  '(move
    (if (f mode)
        (if-else (m drawing)
            (return (l down move))
            (return (n drawing (prev move) move)))))))
```

would be compiled to

```javascript
function compiled(mode, move, down) {
  const move1 = move.pipe(
    startWith(undefined),
    bufferCount(2, 1),
  );
  const r0 = mode.pipe(
    map((mode) => [mode, f(mode)]),
  );
  const r1 = r0.pipe(
    partition(([mode, _temp2]) => _temp2),
  );
  const r2 = r1[0].pipe(
    map(([mode, _temp2]) => [mode, _temp2, h()]),
  );
  const r3 = r2.pipe(
    map(([mode, _temp2, _temp0]) => (drawing) => {
      return _temp0;
    }),
  );
  const r4 = r1[1].pipe(
    map(([mode, _temp2]) => [mode, _temp2, g()]),
  );
  const r5 = r4.pipe(
    map(([mode, _temp2, _temp1]) => (drawing) => {
      return _temp1;
    }),
  );
  const r6 = move1.pipe(
    withLatestFrom(mode),
    map(([[move, prev_move], mode]) => [move, prev_move, mode]),
  );
  const r7 = r6.pipe(
    map(([move, prev_move, mode]) => [move, prev_move, mode, f(mode)]),
  );
  const r8 = r7.pipe(
    filter(([move, prev_move, mode, _temp3]) => _temp3),
  );
  const r9 = r8.pipe(
    withLatestFrom(down),
    map(([[move, prev_move, mode, _temp3], down]) => [move, prev_move, mode, _temp3, down]),
  );
  const r10 = r9.pipe(
    map(([move, prev_move, mode, _temp3, down]) => (drawing) => {
      const _temp2 = m(drawing);
      if (_temp2) {
        return l(down, move);
      } else {
        return n(drawing, prev_move, move);
      }
    }),
  );
  return merge(r3, r5, r10).pipe(
    scan((acc, cur) => cur(acc), undefined),
    filter(Boolean),
  );
}
```

## Documents

See [documents](doc/README.md).