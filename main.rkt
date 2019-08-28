#lang racket

(require "ast/expand.rkt")
(require "ast/monad-desugar.rkt")
(require "test/test-spec.rkt")
(require "print/rxir.rkt")
(require "compile.rkt")

(define (main)
  (let ([spec1
         (spec
          '(mode move down)
          'drawing
          '(f g)
          '()
          '((mode
             (begin
               (let t (g mode))
               (if-else (f t)
                        (if down
                            (return t))
                        (if-else down (return down) (return move)))))
           (move
             (begin
               (let x (g (prev mode)))
               (if x (return x))))))]
        [spec2
         (spec
          '(mode move down)
          'drawing
          '(f g)
          '()
          '((move
             (begin
               (let x (g (prev mode)))
               (if x (return x))))))])
    (println (monad-desugar-spec spec1))
    (println (expand-spec (monad-desugar-spec spec1)))
    (print-rx-program (compile spec1))
    (print-rx-program (compile spec2)))
  (print-rx-program (compile drawing-spec))
  (print-rx-program (compile drawing-custom-spec))
  (print-rx-program (compile drawing-split-spec))

  )

(define (test-action)
  (let ([spec1
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
                                              (return (n drawing (prev move) move))))))
                             (empty-stream))))
            (down (if (g drawing) (return down)))))])
    (print-rx-program (compile spec1))))

                
     