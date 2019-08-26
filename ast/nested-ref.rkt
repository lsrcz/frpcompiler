#lang racket

(provide (all-defined-out))

(struct nested-ref-table (name reflist) #:transparent)
(struct single-ref (name) #:transparent)
(struct sub-ref (name pos) #:transparent)

(define (resolve-ref ref ref-table-list)
  (define (resolve-one ref-table)
    (match ref-table
      [(nested-ref-table name reflist)
       (if (eq? name ref)
           (single-ref ref)
           (let ([index (index-of reflist ref)])
             (if index
                 (sub-ref name index)
                 #f)))]))
  (if (null? ref-table-list)
      #f
      (let ([head-ref (resolve-one (car ref-table-list))])
        (if head-ref
            head-ref
            (resolve-ref ref (cdr ref-table-list))))))

(define empty-ref-table-list '())
(define (add-ref-table name reflist ref-table-list)
  (cons (nested-ref-table name reflist) ref-table-list))
      