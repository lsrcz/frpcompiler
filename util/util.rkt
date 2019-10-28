#lang racket

(provide (all-defined-out))

(define (remove-prev arg)
  (if (list? arg)
      (remove-prev (cadr arg))
      arg))

(define (get-temp-symbol-gen)
 (define num 0)
 (lambda ()
  (let ([ret (string->symbol (format "_temp~a" num))])
   (set! num (+ 1 num))
   ret)))

(define (make-list num v)
  (if (= num 0)
      '()
      (cons v (make-list (- num 1) v))))



