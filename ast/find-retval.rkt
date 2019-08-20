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
          [else (error "not implemented")]))
  (define (return-val-found-deep? return-val inst)
    (or (return-val-found? return-val inst)
        (cond [(bind? inst) (return-val-found-deep? return-val (bind-inst inst))]
              [(return? inst) #f]
              [(if? inst) (return-val-found-deep? return-val (if-branch inst))]
              [(if-else? inst) (or (return-val-found-deep? return-val (if-else-else-branch inst))
                                   (return-val-found-deep? return-val (if-else-then-branch inst)))]
              [else (error "not implemented")])))