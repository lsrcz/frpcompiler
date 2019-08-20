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


(define (compile spec)
  (emit-rxir
   (translate-spec
    (monad-desugar-spec
     (expand-spec spec)))))

(define (print-with-spec rxir-list spec)
  (print-rx-program (spec-inputs spec) rxir-list))


(define (main)
  (let ([spec1
         (spec
          '(mode move down)
          'drawing
          '(f g)
          (list
           '(mode
             (begin
               (let t (g mode))
               (if-else (f t)
                        (if down
                            (return t))
                        (if-else down (return down) (return move)))))
           '(move
             (begin
               (let x (g (prev mode)))
               (if x (return x))))))])
    (print-with-spec (compile spec1) spec1))
  (print-with-spec (compile drawing-spec) drawing-spec))

                
     