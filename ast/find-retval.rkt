#lang racket

(require "../util/util.rkt")
(require "syntax.rkt")

(provide (all-defined-out))

(define (return-val-found? return-val inst)
  (define (iter lst)
    (cond [(null? lst) #f]
          [(not (list? lst)) (eq? lst return-val)]
          [(and (list? lst) (eq? (car lst) 'prev)) (eq? (remove-prev lst) return-val)]
          [else
           (or
            (iter (car lst))
            (iter (cdr lst)))]))
  (cond [(bind? inst) (iter (bind-body inst))]
        [(return? inst) (iter (return-arg inst))]
        [(if? inst) (iter (if-arg inst))]
        [(if-else? inst) (iter (if-else-arg inst))]
        [(custom? inst) #f]
        [(split? inst) #f]
        [else (error "not implemented")]))

(define (return-val-found-imperative? return-val inst)
  (cond [(bind? inst) (return-val-found-imperative? return-val (bind-inst inst))]
        [(if? inst) (return-val-found-imperative? return-val (if-branch inst))]
        [(if-else? inst) (or (return-val-found-imperative? return-val (if-else-then-branch inst))
                             (return-val-found-imperative? return-val (if-else-else-branch inst)))]
        [(empty-stream? inst) #f]
        [(new-stream? inst) (return-val-found-multiple? return-val (map cadr (new-stream-body inst)))]))

(define (return-val-found-deep? return-val inst)
  (or (return-val-found? return-val inst)
      (cond [(bind? inst) (return-val-found-deep? return-val (bind-inst inst))]
            [(return? inst) #f]
            [(if? inst) (return-val-found-deep? return-val (if-branch inst))]
            [(if-else? inst) (or (return-val-found-deep? return-val (if-else-else-branch inst))
                                 (return-val-found-deep? return-val (if-else-then-branch inst)))]
            [(custom? inst) (return-val-found-deep? return-val (custom-body inst))]
            [(split? inst) (return-val-found-imperative? return-val (split-body inst))]
            [else (error "not implemented")])))

(define (return-val-found-multiple? return-val specs)
  (if (null? specs)
      #f
      (or (return-val-found-deep? return-val (car specs))
          (return-val-found-multiple? return-val (cdr specs)))))