#lang racket

(require "ast/expand.rkt")
(require "ast/monad-desugar.rkt")
(require "ir/translate.rkt")
(require "ir/inst.rkt")
(require "test/test-spec.rkt")
(require "print/ir.rkt")
(require "rxir/translate.rkt")
(require "print/rxir.rkt")

(provide (all-defined-out))

(define (trace name x)
  (println (format "trace: ~a: \n" name))
  (println x)
  x)

(define (compile spec)
  (emit-rxir
   (translate-spec
    (expand-spec
     (monad-desugar-spec
      spec)))))

(define (print-with-spec rxir-list spec)
  (print-rx-program (spec-inputs spec) rxir-list))


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
               (if x (return x))))))])
    (println (monad-desugar-spec spec1))
    (println (expand-spec (monad-desugar-spec spec1)))
    (print-with-spec (compile spec1) spec1))
  (print-with-spec (compile drawing-spec) drawing-spec)
  (print-with-spec (compile drawing-custom-spec) drawing-custom-spec)
  (print-with-spec (compile drawing-split-spec) drawing-split-spec)
  )

                
     