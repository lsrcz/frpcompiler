#lang rosette

(require "spec-group-definition.rkt")
(require "../test/test-spec.rkt")
(require "../ast/spec.rkt")


(define (extract-edges spec-input)
  (define (extract-triggers bodys)
    (define (extract-trigger-one-body body)
      (define (extract-trigger-inst inst)
        (match inst
          [(list 'bind _ _ nxt) (extract-trigger-inst nxt)]
          [(list 'if _ branch) (extract-trigger-inst branch)]
          [(list 'if-else _ then-branch else-branch)
           (append (extract-trigger-inst then-branch)
                   (extract-trigger-inst else-branch))]
          [(list 'return _) '()]
          [(cons 'begin begin-body) (extract-trigger-inst (last begin-body))]
          [(list 'new-stream body) (extract-triggers body)]
          [(list 'new-stream-initial body _) (extract-triggers body)]
          [(list 'new-stream-seed body _) (extract-triggers body)]
          [(list 'split _ body) (extract-trigger-inst body)]))
      (cons (car body) (extract-trigger-inst (cadr body))))
    (match bodys
      [(list) '()]
      [(cons body rest) (append (extract-trigger-one-body body) (extract-triggers rest))]))
  (match spec-input
    [(spec inputs output funclist constantlist body)
     (map (lambda (x) (cons x output)) (extract-triggers body))]))

(define (extract-edges-specs specs-input)
  (match specs-input
    [(list) '()]
    [(cons first rest) (append (extract-edges first) (extract-edges-specs rest))]))

(define (check-istree input edges)
  (define (iter visited tovisit)
    (match tovisit
      [(list) #t]
      [(cons nxt rest)
       (if (memq nxt visited)
           #f
           (iter (cons nxt visited) (map cdr (filter (lambda (x) (eq? (car x) nxt)) edges))))]))
  (iter '() (list input)))

(define (check-output-not-in-inputs input edges)
  (null? (filter (lambda (x) (eq? (cdr x) input)) edges)))

(define (check-spec-group spec-group-input)
  (match spec-group-input
    [(spec-group externals specs)
     (let ([edges (extract-edges-specs specs)])
       (null? (filter (lambda (x) (not x))
                      (map (lambda (x)
                             (and (check-output-not-in-inputs x edges)
                                  (check-istree x edges))) externals))))]))

(module+ test
  (require rackunit)
  (displayln (extract-edges drawing-split-spec))
  (displayln (extract-edges drawing-spec))
  (check-equal?
   (check-spec-group
    (spec-group '(a b)
                (list
                 (spec '(a) 'c '() '()
                       '((a (return a))))
                 (spec '(c) 'a '() '()
                       '((c (return c)))))))
   #f)
  (check-equal?
   (check-spec-group
    (spec-group '(a b)
                (list
                 (spec '(a) 'c '() '()
                       '((a (return a))))
                 (spec '(c) 'b '() '()
                       '((c (return c)))))))
   #f)
  (check-equal?
   (check-spec-group
    (spec-group '(a b)
                (list
                 (spec '(a) 'c '() '()
                       '((a (return a))))
                 (spec '(c) 'd '() '()
                       '((c (return c))))
                 (spec '(d) 'c '() '()
                       '((d (return d)))))))
   #f)
  )
