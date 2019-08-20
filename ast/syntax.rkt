#lang racket

(provide (all-defined-out))

(define get-op car)
(define if-arg cadr)
(define if-branch caddr)
(define if-else-arg cadr)
(define if-else-then-branch caddr)
(define if-else-else-branch cadddr)
(define begin-seq cdr)
(define return-arg cadr)
(define let-body caddr)
(define let-name cadr)
(define bind-body caddr)
(define bind-name cadr)
(define bind-inst cadddr)


(define (build-begin seq) (cons 'begin seq))
(define (build-let sym val) (list 'let sym val))
(define (build-bind sym val nxt) (list 'bind sym val nxt))
(define (build-if arg branch) (list 'if arg branch))
(define (build-if-else arg then-branch else-branch)
  (list 'if-else arg then-branch else-branch))
(define (build-return arg) (list 'return arg))


(define (if? body) (eq? (get-op body) 'if))
(define (if-else? body) (eq? (get-op body) 'if-else))
(define (begin? body) (eq? (get-op body) 'begin))
(define (return? body) (eq? (get-op body) 'return))
(define (let? body) (eq? (get-op body) 'let))
(define (bind? body) (eq? (get-op body) 'bind))
