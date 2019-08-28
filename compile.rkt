#lang racket

(require "ast/expand.rkt")
(require "ast/monad-desugar.rkt")
(require "ir/translate.rkt")
(require "rxir/translate.rkt")

(provide compile)

(define (compile spec)
  (emit-rxir
   (translate-spec
    (expand-spec
     (monad-desugar-spec
      spec)))))