#lang racket

(require "../test/test-spec.rkt")
(require "spec.rkt")
(require "syntax.rkt")
(require rackunit)

(provide check-spec)

(define (check-begin-let body)
  (define (begin-visitor . lst)
    (let ([cur (car lst)]
          [rest (cdr lst)])
      (if (null? rest)
          (and (not (pair? cur))
               cur)
          (and (pair? cur)
               (eq? (car cur) 'let)
               (apply begin-visitor rest)))))
  (define (collect-new-stream l)
    (if (null? l)
        #t
        (and (cadar l) (collect-new-stream (cdr l)))))
  (define descend-list
    (list
      (cons 'if (lambda (_ branch) branch))
      (cons 'if-else (lambda (_ then-branch else-branch) (and then-branch else-branch)))
      (cons 'begin begin-visitor)
      (cons 'empty-stream (lambda () #t))
      (cons 'new-stream collect-new-stream)
      (cons 'new-stream-initial (lambda (lst _) (collect-new-stream lst)))
      (cons 'new-stream-seed (lambda (lst _) (collect-new-stream lst)))
      (cons 'bind (lambda (_ _1 body) body))
      (cons 'return (lambda (_) #t))
      (cons 'split (lambda (_ body) body))
      (cons 'custom (lambda (_ body) body))
    ))
  (define check-visitor
    (visitor
     descend-list
     '()
     descend-list
     '()))
  (if (null? body)
      #t
      (let* ([cur (car body)]
             [rest (cdr body)]
             [cur-body (cadr cur)])
        (and (visit check-visitor cur-body #f) (check-begin-let rest)))))

(define (check-unique-split body)
  (define (check-unique-split-inner outter body)
    (define (check-unique-split-imp-inst outter inst)
      (match inst
        [(list 'if _ branch) (check-unique-split-imp-inst outter branch)]
        [(list 'if-else _ then-branch else-branch)
         (and (check-unique-split-imp-inst outter then-branch)
              (check-unique-split-imp-inst outter else-branch))]
        [(cons 'begin lst) (check-unique-split-imp-inst outter (last lst))]
        [(list 'empty-stream) #t]
        [(list 'new-stream body) (check-unique-split-inner outter body)]
        [(list 'new-stream-initial body _) (check-unique-split-inner outter body)]
        [(list 'new-stream-seed body _) (check-unique-split-inner outter body)]))
    (define (check-unique-split-inst outter possible inst)
      (match inst
        [(list 'if _ branch) (check-unique-split-inst outter possible branch)]
        [(list 'if-else _ then-branch else-branch)
         (and (check-unique-split-inst outter #f then-branch)
              (check-unique-split-inst outter #f else-branch))]
        [(list 'bind _ _ inst) (check-unique-split-inst outter possible inst)]
        [(list 'return _) #t]
        [(list 'split _ body) (and possible (check-unique-split-imp-inst outter body))]
        [(list 'custom _ body) (check-unique-split-inst outter possible inst)]
        [(cons 'begin lst) (check-unique-split-inst outter possible (last lst))]))  
    (define (intersect-list list1 list2)
      (if (null? list1)
          #f
          (if (member (car list1) list2)
              #t
              (intersect-list (cdr list1) list2))))
    (define (iter outter possible instlist)
      (if (null? instlist)
          #t
          (and (check-unique-split-inst outter possible (car instlist))
               (iter outter possible (cdr instlist)))))
    (let ([from-list (map car body)]
          [inst-list (map cadr body)])
      (if (intersect-list from-list outter)
          #f
          (let ([new-outter (append from-list outter)])
            (iter new-outter (eq? (length inst-list) 1) inst-list)))))
  (check-unique-split-inner '() body))

(define (check-spec-body body)
  (and (check-begin-let body) (check-unique-split body)))

(define (check-spec spec-input)
  (match spec-input
    [(spec _ _ _ _ _ body) (check-spec-body body)]))

(define (main)
  (check-equal? (check-begin-let
                 '((a (begin (let x a) (if a (return b)))))) #t)
  (check-equal? (check-begin-let
                 '((a (begin (let x a) (if a (return b)) (return a))))) #f)
  (check-equal? (check-begin-let
                 '((a (begin (let x a)  (return a))))) #t)
  (check-equal? (check-begin-let
                 '((a (begin (let x a) )))) #f)
  (check-equal?
   (check-begin-let
    '((a (split (()) (if-else a (new-stream ((b (begin (let x b) (return x))))) (empty-stream))))))
   #t)
  (check-equal?
   (check-begin-let
    '((a (split (()) (if-else a (new-stream ((b (begin (let x b))))) (empty-stream))))))
   #f)
  (check-equal?
   (check-begin-let
    '((a (split (()) (if-else a (new-stream ((b (begin (return x))))) (empty-stream))))))
   #t)
  (check-equal?
   (check-begin-let
    '((a (split (()) (if-else a (new-stream ((b (begin (return x) (return x))))) (empty-stream))))))
   #f)
  (check-equal? (check-spec-body
                 '((a (split (()) (empty-stream))))) #t)
  (check-equal? (check-spec-body
                 '((a (split (()) (new-stream ((a (return a)))))))) #f)
  (check-equal? (check-spec-body
                 '((a (split (()) (empty-stream))) (b (return b)))) #f)
  (check-equal? (check-spec-body
                 '((a (split (()) (new-stream ((a (return a)))))))) #f)
  (check-equal? (check-spec-body
                 '((a (if-else a (split (()) (empty-stream)) (return a))))) #f)
  )
(main)
