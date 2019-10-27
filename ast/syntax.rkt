#lang rosette/safe

(provide (all-defined-out))

(define get-op car)
(define if-arg cadr)
(define if-branch caddr)
(define if-else-arg cadr)
(define if-else-then-branch caddr)
(define if-else-else-branch cadddr)
(define if-multi-args cadr)
(define if-multi-branch caddr)
(define if-multi-mapping cadddr)
(define if-else-multi-args cadr)
(define if-else-multi-then-branch caddr)
(define if-else-multi-else-branch cadddr)
(define if-else-multi-mapping (lambda (x) (car (cddddr x))))
(define case-multi-args cadr)
(define case-multi-branchs caddr)
(define case-multi-mapping cadddr)
(define begin-seq cdr)
(define return-arg cadr)
(define let-body caddr)
(define let-name cadr)
(define bind-body caddr)
(define bind-name cadr)
(define bind-inst cadddr)
(define custom-name cadr)
(define custom-body caddr)
(define split-bindings cadr)
(define split-body caddr)
(define split-binding-name car)
(define split-binding-body cadr)
(define new-stream-body cadr)
(define new-stream-initial-body cadr)
(define new-stream-initial-initial caddr)
(define new-stream-seed-body cadr)
(define new-stream-seed-seed caddr)


(define (build-begin seq) (cons 'begin seq))
(define (build-let sym val) (list 'let sym val))
(define (build-bind sym val nxt) (list 'bind sym val nxt))
(define (build-if arg branch) (list 'if arg branch))
(define (build-if-else arg then-branch else-branch)
  (list 'if-else arg then-branch else-branch))
(define (build-if-multi args branch mapping) (list 'if-multi args branch mapping))
(define (build-if-else-multi args then-branch else-branch mapping)
  (list 'if-else-multi args then-branch else-branch mapping))
(define (build-case-multi args branchs mapping)
  (list 'case-multi args branchs mapping))
(define (build-return arg) (list 'return arg))
(define (build-custom name inst) (list 'custom name inst))
(define (build-split bindings body) (list 'split bindings body))
(define (build-new-stream body) (list 'new-stream body))
(define (build-new-stream-initial body initial) (list 'new-stream-initial body initial))
(define (build-new-stream-seed body seed) (list 'new-stream-seed body seed))
(define (build-empty) (list 'empty-stream))
(define (build-return-empty) (list 'return-empty))


(define (if? body) (eq? (get-op body) 'if))
(define (if-else? body) (eq? (get-op body) 'if-else))
(define (if-multi? body) (eq? (get-op body) 'if-multi))
(define (if-else-multi? body) (eq? (get-op body) 'if-else-multi))
(define (case-multi? body) (eq? (get-op body) 'case-multi))
(define (begin? body) (eq? (get-op body) 'begin))
(define (return? body) (eq? (get-op body) 'return))
(define (let? body) (eq? (get-op body) 'let))
(define (bind? body) (eq? (get-op body) 'bind))
(define (custom? body) (eq? (get-op body) 'custom))
(define (split? body) (eq? (get-op body) 'split))
(define (new-stream? body) (eq? (get-op body) 'new-stream))
(define (new-stream-initial? body) (eq? (get-op body) 'new-stream-initial))
(define (new-stream-seed? body) (eq? (get-op body) 'new-stream-seed))
(define (empty-stream? body) (eq? (get-op body) 'empty-stream))
(define (return-empty? body) (eq? (get-op body) 'return-empty))


(struct visitor (descend-list stopping-list descend-list-imp stopping-list-imp) #:transparent)
(define (visit visitor-input body imp)
  ; begin excluded
  (define descend-spec
    (list '(if n y)
          '(if-else n y y)
          '(if-multi n y n)
          '(if-else-multi n y y n)
          '(case-multi n m n)
          '(return n)
          '(bind n n y)
          '(custom n y)
          '(split n yc)
          '(new-stream sc)
          '(new-stream-initial sc n)
          '(empty-stream)
          '(return-empty)
          '(let n n)
          '(new-stream-seed sc n)))
  (define (map-body-args body)
    (define (map-begin-args args)
      (map (lambda (x) (visit visitor-input x imp)) args))
    (define (map-args sp args)
      (if (null? sp)
          '()
          (cons
           (cond [(eq? 'n (car sp))
                  (car args)]
                 [(eq? 'y (car sp))
                  (visit visitor-input (car args) imp)]
                 [(eq? 'yc (car sp))
                  (visit visitor-input (car args) (not imp))]
                 [(eq? 'm (car sp))
                  (map (lambda (x) (visit visitor-input x imp)) (car args))]
                 [(eq? 'mc (car sp))
                  (map (lambda (x) (visit visitor-input x (not imp))) (car args))]
                 [(eq? 'sc (car sp))
                  (map (lambda (x) (list (car x) (visit visitor-input (cadr x) (not imp)))) (car args))])
           (map-args (cdr sp) (cdr args)))))
    (if (begin? body)
        (map-begin-args (cdr body))
        (let ([sp (assoc (car body) descend-spec)])
          (map-args (cdr sp) (cdr body)))))
  (let ([descend-list
         ((if imp
              visitor-descend-list-imp
              visitor-descend-list)
          visitor-input)]
        [stopping-list
         ((if imp
              visitor-stopping-list-imp
              visitor-stopping-list)
          visitor-input)])
    (let* ([d (assoc (car body) descend-list)]
           [s (assoc (car body) stopping-list)])
      (cond [d (apply (cdr d) (map-body-args body))]
            [s (apply (cdr s) (cdr body))]
            [else body]))))