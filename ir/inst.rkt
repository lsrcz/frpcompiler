#lang racket

(provide (all-defined-out))

(struct input-inst (name num shape) #:transparent)
(struct intro-inst (intro-lst ref shape) #:transparent)
(struct compute-inst (to-compute name ref shape) #:transparent)
(struct filter-inst (arg ref shape) #:transparent)
(struct partition-inst (arg ref shape) #:transparent)
(struct ret-inst (arg ref) #:transparent)
(struct merge-inst (to-merge) #:transparent)
(struct merge-action-inst (to-merge) #:transparent)
(struct ret-action-inst (return-val action ref) #:transparent)

(define (get-shape inst)
  (match inst
    [(input-inst _ _ shape) shape]
    [(intro-inst _ _ shape) shape]
    [(compute-inst _ _ _ shape) shape]
    [(filter-inst _ _ shape) shape]
    [(cons (partition-inst _ _ shape) _) shape]
    [else (error "no shape")]))

