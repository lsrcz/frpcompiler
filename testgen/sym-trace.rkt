#lang rosette

(require "../interpret/environment.rkt"
         rosette/lib/match
         rosette/lib/angelic)

(provide (all-defined-out))

(define (get-single-symbolic-event name constructor)
  (event name (constructor)))

(define (get-symbolic-event constructor-mapping)
  (if (null? constructor-mapping)
      (empty-event)
      (let ([cur (car constructor-mapping)]
            [rest (cdr constructor-mapping)])
        (match cur
          [(cons name constructor)
           (choose* (get-single-symbolic-event name constructor) (get-symbolic-event rest))]))))

(define (get-symbolic-event-list constructor-mapping length)
  (for/list ([i length])
    (get-symbolic-event constructor-mapping)))

(define (get-symbolic-trace constructor-mapping length)
  (trace (get-symbolic-event-list constructor-mapping length)))

(define (integer-constructor)
  (define-symbolic* i integer?)
  i)
(define (boolean-constructor)
  (define-symbolic* b boolean?)
  b)


(define (main)
  (error-print-width 10000000)
  (define mapping
    (list
     (cons 'a integer-constructor)
     (cons 'b boolean-constructor)))
  (define x (get-symbolic-event-list mapping 1))
  (define m (synthesize #:forall (list)
                        #:guarantee (assert (eq? (event-name (car x)) 'a))))
  (displayln (evaluate x m)))