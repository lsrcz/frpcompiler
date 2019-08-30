#lang racket

(require "ast/extract.rkt")
(require "ast/monad-desugar.rkt")
(require "ir/translate.rkt")
(require "rxir/translate.rkt")

(provide compile)

(define (compile spec)
  (emit-rxir
   (translate-spec
    (extract-spec
     (monad-desugar-spec
      spec)))))