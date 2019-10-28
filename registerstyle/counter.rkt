#lang rosette

(require "model.rkt")
(require "dsl.rkt")
(require "sketch.rkt")

(define stream-length 4)

(define sym-inc (new-event-stream get-sym-int stream-length))
(define sym-dec (new-event-stream get-sym-int stream-length))

(define (target-function inc dec) (rxScan + (rxMerge (rxMap (λ (i) 1) inc)
                                                     (rxMap (λ (i) -1) dec))))

(define sym-sk (make-symbolic-sketch 4 2))

(define evaled-sk ((get-sketch-function sym-sk) sym-inc sym-dec))



(define b (time (synthesize #:forall (symbolics (list sym-inc sym-dec))
                            #:guarantee (assert (equal? evaled-sk (target-function sym-inc sym-dec))))))
(displayln (evaluate sym-sk b))
(if (unsat? b)
    (println "Synthesis from reference function: unsat")
    (print-sketch sym-sk b))