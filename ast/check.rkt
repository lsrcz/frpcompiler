#lang racket

(require "../test/test-spec.rkt")
(require "spec.rkt")
(require rackunit)

(provide check-spec)

(define (check-begin-let body)
  (define (check-begin-let-begin last-func lst)
    (let ([cur (car lst)]
          [rest (cdr lst)])
      (if (null? rest)
          (and (not (eq? (car cur) 'let))
               (last-func cur))
          (and (eq? (car cur) 'let)
               (check-begin-let-begin last-func rest)))))
  (define (check-begin-let-imperative inst)
    (match inst
      [(list 'if _ branch) (check-begin-let-imperative branch)]
      [(list 'if-else _ then-branch else-branch)
       (and (check-begin-let-imperative then-branch)
            (check-begin-let-imperative else-branch))]
      [(cons 'begin lst) (check-begin-let-begin check-begin-let-imperative lst)]
      [(list 'empty-stream) #t]
      [(list 'new-stream body) (check-begin-let body)]
      [(list 'new-stream-initial body _) (check-begin-let body)]
      [(list 'new-stream-seed body _) (check-begin-let body)]))
  (define (check-begin-let-one inst)
    (match inst
      [(list 'if _ branch) (check-begin-let-one branch)]
      [(list 'if-else _ then-branch else-branch)
       (and (check-begin-let-one then-branch)
            (check-begin-let-one else-branch))]
      [(list 'bind _ _ inst) (check-begin-let-one inst)]
      [(list 'return _) #t]
      [(list 'split _ body) (check-begin-let-imperative body)]
      [(list 'custom _ inst) (check-begin-let-one inst)]
      [(cons 'begin lst) (check-begin-let-begin check-begin-let-one lst)]))
  (if (null? body)
      #t
      (and (check-begin-let-one (cadar body)) (check-begin-let (cdr body)))))

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
    [(spec _ _ _ _ body) (check-spec-body body)]))

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

