#lang rosette/safe

(require "syntax.rkt")
(require "spec.rkt")
(require rackunit)

(provide (all-defined-out))

(struct boolean-func (name argnum) #:transparent)
(struct int-func (name argnum branchnum) #:transparent)

(define (collect-select-func spec)
  (define descend-list
    (list
      (cons 'if (lambda (_ branch) branch))
      (cons 'if-else (lambda (_ then-branch else-branch)
                       (append then-branch else-branch)))
      (cons 'if-multi (lambda (args branch mapping)
                        (cons (boolean-func mapping (length args)) branch)))
      (cons 'if-else-multi (lambda (args then-branch else-branch mapping)
                             (cons (boolean-func mapping (length args)) (append then-branch else-branch))))
      (cons 'case-multi
        (lambda (args branchs mapping)
          (cons (int-func mapping (length args) (length branchs))
            (append* branchs))))
      (cons 'begin (lambda lst (last lst)))
      (cons 'return (lambda () '()))
      (cons 'return-empty (lambda () '()))
      (cons 'custom (lambda (_ body) body))
      (cons 'split (lambda (_ body) body))
      (cons 'new-stream (lambda (lst) (append* (map cadr lst))))
      (cons 'new-stream-initial (lambda (lst _) (append* (map cadr lst))))
      (cons 'new-stream-seed (lambda (lst _) (append* (map cadr lst))))
      (cons 'empty-stream (lambda () '()))
      (cons 'bind (lambda (_ _1 inst) inst))
    ))
  (append* (map
    (lambda (x)
      (displayln (cadr x))
      (visit (visitor descend-list '() descend-list '()) (cadr x) #f))
    (spec-body spec))))

(define (main)
  (define test-spec (spec '(a b) 'c '(f1 f2) '(a1 b1) '()
    '((a (case-multi ((eq? a a1) (eq? a a1)) ((if-multi ((eq? b b1) (eq? b b1)) (return-empty) f2)) f1)))))
  (displayln (collect-select-func test-spec))
)
(main)
