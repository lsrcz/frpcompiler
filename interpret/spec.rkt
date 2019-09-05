#lang rosette/safe

(require "environment.rkt")
(require "../test/test-spec.rkt")
(require "analyzed.rkt")
(require rackunit)
(require rosette/lib/match)
(require rosette/base/struct/struct)

(provide interpret-spec)

(define (die) (car '()))

(define (analyze-if-imp js-expr branch)
  (let ([analyzed-js-expr (analyze-js-expr js-expr #t)]
        [analyzed-branch (analyze-inst-imp branch)])
    (analyzed-value
     (lambda (env)
       (if (analyzed-js-expr env)
           ((analyzed-value-call analyzed-branch) env)
           env))
     (analyzed-value-unsub analyzed-branch))))

(define (analyze-if-else-imp js-expr then-branch else-branch)
  (let ([analyzed-js-expr (analyze-js-expr js-expr #t)]
        [analyzed-then-branch (analyze-inst-imp then-branch)]
        [analyzed-else-branch (analyze-inst-imp else-branch)])
    (analyzed-value
     (lambda (env)
       (if (analyzed-js-expr env)
           ((analyzed-value-call analyzed-then-branch) env)
           ((analyzed-value-call analyzed-else-branch) env)))
     (lambda (env)
        ((analyzed-value-unsub analyzed-else-branch) ((analyzed-value-unsub analyzed-then-branch) env))))))

(define (analyze-bind-imp name js-expr next)
  (let ([analyzed-js-expr (analyze-js-expr js-expr #t)]
        [analyzed-next (analyze-inst-imp next)])
    (analyzed-value
     (lambda (env)
       (let ([value (analyzed-js-expr env)])
         ((analyzed-value-call analyzed-next) (add-const-binding name value env))))
     (analyzed-value-unsub analyzed-next))))

(define (analyze-begin-imp lst)
  (let* ([let-list (reverse (cdr (reverse lst)))]
         [next (last lst)])
    (let ([let-trans-list
           (map (match-lambda
                  [(list 'let name js-expr)
                   (let ([analyzed-js-expr (analyze-js-expr js-expr #t)]) (binding name analyzed-js-expr))])
                let-list)]
          [analyzed-next (analyze-inst-imp next)])
      (define (iter let-trans-list env)
        (if (null? let-trans-list)
            ((analyzed-value-call analyzed-next) env)
            (let ([cur (car let-trans-list)]
                  [rest (cdr let-trans-list)])
              (let ([val ((binding-value cur) env)])
              (if (too-early? val)
                  env
                  (iter rest (add-const-binding (binding-name cur) val env)))))))
      (analyzed-value
       (lambda (env)
         (iter let-trans-list env))
       (analyzed-value-unsub next)))))

(define (analyze-empty-stream-inst)
  (analyzed-value
   (lambda (env) env)
   (lambda (env) env)))

(define (analyze-new-stream-inst body)
  (let ([analyzed-new-stream (analyze-new-stream body)])
    (analyzed-value
     (lambda (env) (set-ret-value 'undefined (subscribe analyzed-new-stream env)))
     (lambda (env) (unsubscribe analyzed-new-stream env)))))

(define (analyze-new-stream-initial-inst body initial)
  (let ([analyzed-new-stream (analyze-new-stream body)]
        [analyzed-js-expr (analyze-js-expr initial)])
    (analyzed-value
     (lambda (env)
       (set-ret-value (analyzed-js-expr env) (subscribe analyzed-new-stream env)))
     (lambda (env) (unsubscribe analyzed-new-stream env)))))

(define (analyze-new-stream-seed-inst body seed)
  (let ([analyzed-new-stream (analyze-new-stream body)]
        [analyzed-js-expr (analyze-js-expr seed)])
    (analyzed-value
     (lambda (env)
       (clear-ret-value (set-ret-value (analyzed-js-expr env) (subscribe analyzed-new-stream env))))
     (lambda (env) (unsubscribe analyzed-new-stream env)))))

(define (analyze-new-stream body)
  (let ([analyzed-body (map (match-lambda
                              [(list name inst) (list name (analyze-inst inst))]) body)])
    (analyzed-value
     (lambda (env)
       (let ([evt (get-event env)])
         (match evt
           [(event name value)
            (let ([to-run (assoc name analyzed-body)])
              (if to-run
                  ((analyzed-value-call (cadr to-run)) env)
                  env))]
           [(empty-event) env])))
     (lambda (env)
       (foldl (lambda (func env) (func env)) env
              (map (lambda (x) (analyzed-value-unsub (cadr x))) analyzed-body))))))
   

(define (analyze-inst-imp inst)
  (match inst
    [(list 'if js-expr branch)
     (analyze-if-imp js-expr branch)]
    [(list 'if-else js-expr then-branch else-branch)
     (analyze-if-else-imp js-expr then-branch else-branch)]
    [(list 'bind name js-expr next)
     (analyze-bind-imp name js-expr next)]
    [(cons 'begin lst)
     (analyze-begin-imp lst)]
    [(list 'empty-stream)
     (analyze-empty-stream-inst)]
    [(list 'new-stream body)
     (analyze-new-stream-inst body)]
    [(list 'new-stream-initial body initial)
     (analyze-new-stream-initial-inst body initial)]
    [(list 'new-stream-seed body seed)
     (analyze-new-stream-seed-inst body seed)]))
    ;[_ (error "should not happen")]))

(define (analyze-js-expr js-expr [only-constant #f])
  (match js-expr
    [(list 'prev _)
     (lambda (env)
       (let ([result (resolve-environment env js-expr only-constant)])
         (cond [(resolved? result)
                (resolved-value result)]
               [(not-found? result) (die)]
               [(too-early? result) result])))]
    [(cons f lst)
     (let ([analyzed-f (analyze-js-expr f)]
           [analyzed-lst (map analyze-js-expr lst)])
       (lambda (env)
         (let ([analyzed-f-val (analyzed-f env)]
               [analyzed-lst-val (map (lambda (x) (x env)) analyzed-lst)])
           (if (or (not-found? analyzed-f-val)
                   (not (null? (filter not-found? analyzed-lst-val))))
               (die)
               (if (or (too-early? analyzed-f-val)
                       (not (null? (filter too-early? analyzed-lst-val))))
                   (too-early)
                   (apply analyzed-f-val analyzed-lst-val))))))]
    [_
     (lambda (env)
       (let ([result (resolve-environment env js-expr only-constant)])
         (cond [(resolved? result)
                (resolved-value result)]
               [(not-found? result) (die)]
               [(too-early? result) result])))]))



(define (analyze-if js-expr branch)
  (let ([analyzed-js-expr (analyze-js-expr js-expr)]
        [analyzed-branch (analyze-inst branch)])
    (analyzed-value
     (lambda (env)
       (let ([js-val (analyzed-js-expr env)])
         (if (too-early? js-val)
             env
             (if js-val
                 ((analyzed-value-call analyzed-branch) env)
                 env))))
     (analyzed-value-unsub analyzed-branch))))

(define (analyze-if-else js-expr then-branch else-branch)
  (let ([analyzed-js-expr (analyze-js-expr js-expr)]
        [analyzed-then (analyze-inst then-branch)]
        [analyzed-else (analyze-inst else-branch)])
    (analyzed-value
     (lambda (env)
       (let ([js-val (analyzed-js-expr env)])
         (if (too-early? js-val)
             env
             (if js-val
                 ((analyzed-value-call analyzed-then) env)
                 ((analyzed-value-call analyzed-else) env)))))
     (lambda (env)
        ((analyzed-value-unsub analyzed-else) ((analyzed-value-unsub analyzed-then) env))))))

(define (analyze-begin lst)
  (let* ([let-list (reverse (cdr (reverse lst)))]
         [next (last lst)])
    (let ([let-trans-list
           (map (match-lambda
                  [(list 'let name js-expr)
                   (let ([analyzed-js-expr (analyze-js-expr js-expr)]) (binding name analyzed-js-expr))])
                let-list)]
          [analyzed-next (analyze-inst next)])
      (define (iter let-trans-list env)
        (if (null? let-trans-list)
            ((analyzed-value-call analyzed-next) env)
            (let ([cur (car let-trans-list)]
                  [rest (cdr let-trans-list)])
              (let ([val ((binding-value cur) env)])
              (if (too-early? val)
                  env
                  (iter rest (add-stream-binding (binding-name cur) val env)))))))
      (analyzed-value
       (lambda (env)
         (iter let-trans-list env))
       (analyzed-value-unsub analyzed-next)))))

(define (analyze-bind name js-expr next)
  (let ([analyzed-js-expr (analyze-js-expr js-expr)]
        [analyzed-next (analyze-inst next)])
    (analyzed-value
     (lambda (env)
       (let ([value (analyzed-js-expr env)])
         (if (too-early? value)
             env
             ((analyzed-value-call analyzed-next) (add-stream-binding name value env)))))
     (analyzed-value-unsub analyzed-next))))

(define (analyze-return js-expr)
  (let ([analyzed-js-expr (analyze-js-expr js-expr)])
    (analyzed-value
     (lambda (env)
       (let ([value (analyzed-js-expr env)])
         (if (too-early? value)
             env
             (set-ret-value value env))))
     (lambda (env) env))))

(define (analyze-split bindings body)
  (let ([binding-list (map (match-lambda [(list name js-expr) (list name (analyze-js-expr js-expr))]) bindings)]
        [analyzed-body (analyze-inst-imp body)])
    (define (iter binding-list env)
      (match binding-list
        [(list) ((analyzed-value-call analyzed-body) env)]
        [(list-rest (list name analyzed-js-expr) rest)
         (let ([value (analyzed-js-expr env)])
           (if (too-early? value)
               env
               (iter rest (add-const-binding name (analyzed-js-expr env) env))))]))
    (analyzed-value
     (lambda (env)
       (iter binding-list ((analyzed-value-unsub analyzed-body) env)))
     (analyzed-value-unsub analyzed-body))))

(define (analyze-inst inst)
  (match inst
    [(list 'if js-expr branch)
     (analyze-if js-expr branch)]
    [(list 'if-else js-expr then-branch else-branch)
     (analyze-if-else js-expr then-branch else-branch)]
    [(list 'return js-expr)
     (analyze-return js-expr)]
    [(list 'bind name js-expr next)
     (analyze-bind name js-expr next)]
    [(cons 'begin lst)
     (analyze-begin lst)]
    [(list 'split bindings body)
     (analyze-split bindings body)]))

(define (interpret-spec spec-input trace bindings)
  (define (advance glb-env)
    (define (iter active-sub glb-env)
      (match active-sub
        [(list) glb-env]
        [(cons (sub bindings body) rest)
         (let ([env (environment glb-env (local-env '() bindings))])
           (match ((analyzed-value-call body) env)
             [(environment new-glb-env _) (iter (cdr active-sub) new-glb-env)]))]))
    (match glb-env
      [(global-env _ _ _ _ _ _ active-sub) (advance-time-glb (iter active-sub glb-env))]))
  (define (construct-list glb-env)
    (match glb-env
      [(global-env trace time _ _ _ _ _)
       (if (eq? (length (trace-event-lst trace)) time)
           '()
           (let ([new-glb-env (advance (struct-copy global-env glb-env [return-val 'undefined]))])
             (match new-glb-env
               [(global-env _ _ _ output _ return-val _)
                (cons (if (undefined? return-val) (empty-event) (event output return-val)) (construct-list new-glb-env))])))]))
  (match spec-input
    [(spec inputs output _ _ body)
     (let ([glb-env (global-env trace 0 inputs output 'undefined 'undefined (list (sub bindings (analyze-new-stream body))))])
       (construct-list glb-env))]))
     
     

(define (main)
  (define spec1 (spec '(a d) 'b '(f) '(t) '((a (if d (return (f a (prev a))))))))
  (define spec2 (spec '(a d) 'b '(f) '(t) '((a (if-else d (return (f a (prev a))) (return (g a (prev a))))))))
  (define spec3 (spec '(a d) 'b '(f) '(t) '((a (if-else d (return (f a (prev a))) (return a))))))
  (define spec4 (spec '(a d) 'b '(f) '(t) '((a (if-else d (return a) (return (f a (prev a))))))))
  (define spec5 (spec '(a d) 'b '(f) '(t) '((a (if-else (not d) (return a) (return (f a (prev a))))))))
  (define spec6 (spec '(a d) 'b '(f) '(t) '((a (bind t (not d) (if-else t (return a) (return (f a (prev a)))))))))
  (define spec7 (spec '(a d) 'b '(f) '(t) '((a (bind t (not d) (bind x (not (not t)) (if-else x (return a) (return (f a (prev a))))))))))
  (define spec8 (spec '(a d) 'b '(f) '(t) '((a (begin (let t (not d))
                                                      (let x (not (not t)))
                                                      (if-else x (return a) (return (f a (prev a)))))))))
  (define tr1
    (trace
     (list
      (event 'd #t)
      (event 'a 1)
      (event 'a 2)
      (event 'd #f)
      (event 'a 3)
      (event 'd #t)
      (event 'a 4)
      (event 'a 5))))
  (define binding1 (list (binding 'f +) (binding 'g -) (binding 'not not)))
  (check-equal? (interpret-spec spec1 tr1 binding1)
                (list
                 (empty-event)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (empty-event)
                 (empty-event)
                 (event 'b 7)
                 (event 'b 9)))
  (check-equal? (interpret-spec spec2 tr1 binding1)
                (list
                 (empty-event)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 1)
                 (empty-event)
                 (event 'b 7)
                 (event 'b 9)))
  (check-equal? (interpret-spec spec3 tr1 binding1)
                (list
                 (empty-event)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 7)
                 (event 'b 9)))
  (check-equal? (interpret-spec spec4 tr1 binding1)
                (list
                 (empty-event)
                 (event 'b 1)
                 (event 'b 2)
                 (empty-event)
                 (event 'b 5)
                 (empty-event)
                 (event 'b 4)
                 (event 'b 5)))
  (check-equal? (interpret-spec spec5 tr1 binding1)
                (list
                 (empty-event)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 7)
                 (event 'b 9)))
  (check-equal? (interpret-spec spec6 tr1 binding1)
                (list
                 (empty-event)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 7)
                 (event 'b 9)))
  (check-equal? (interpret-spec spec7 tr1 binding1)
                (list
                 (empty-event)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 7)
                 (event 'b 9)))
  (check-equal? (interpret-spec spec8 tr1 binding1)
                (list
                 (empty-event)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (event 'b 7)
                 (event 'b 9)))

  (define spec9 (spec '(a b c) 'd '(add1 + - not >) '(t x)
                      '((a (split ((a1 (add1 a)))
                                  (if (> a1 t)
                                           (new-stream ((b (split ((b1 (add1 b)))
                                                                  (if-else (> b1 x) (new-stream ((c (return (+ a1 b1 c))))) (empty-stream))))))))))))
  (define spec10 (spec '(a b c) 'd '(add1 + - not >) '(t x)
                      '((a (split ((a1 (add1 a)))
                                  (if (> a1 t)
                                           (new-stream-initial ((b (split ((b1 (add1 b)))
                                                                          (if-else (> b1 x) (new-stream ((c (return (+ a1 b1 c))))) (empty-stream)))))
                                                               a1)))))))
  (define tr2
    (trace
     (list
      (event 'c 1)
      (event 'b 0)
      (event 'c 2)
      (event 'b 1)
      (event 'c 3)
      (event 'a 0)
      (event 'c 1)
      (event 'b 0)
      (event 'c 2)
      (event 'b 1)
      (event 'c 3)
      (event 'a 1)
      (event 'c 1)
      (event 'b 0)
      (event 'c 2)
      (event 'b 1)
      (event 'c 3)
      (event 'a 0)
      (event 'c 3)

      )))
  (define binding2
    (list
     (binding 'add1 add1)
     (binding '+ +)
     (binding '- -)
     (binding 'not not)
     (binding 'undefined? undefined?)
     (binding '> >)
     (binding 't 1)
     (binding 'x 1)
     (binding 'undefined 'undefined)))
  (check-equal? (interpret-spec spec9 tr2 binding2)
                (list (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (event 'd 7)
                      (empty-event)
                      (empty-event))
                )
  (check-equal? (interpret-spec spec10 tr2 binding2)
                (list (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (event 'd 2)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (event 'd 7)
                      (empty-event)
                      (empty-event))
                )

  (define spec11 (spec '(a b) 'd '(add1 + - not >) '(t x)
                       '((a (split ((a1 (add1 a)))
                                   (if (> a1 t)
                                       (new-stream-initial ((b (return (+ b d))))      
                                                           a1)))))))
  (define tr3
    (trace
     (list
      (event 'b 10)
      (event 'b 11)
      (event 'a 0)
      (event 'b 10)
      (event 'b 11)
      (event 'a 1)
      (event 'b 10)
      (event 'b 11)
      (event 'a 2)
      (event 'b 10)
      (event 'b 11)
      )))
  (check-equal? (interpret-spec spec11 tr3 binding2)
                (list (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (event 'd 2)
                      (event 'd 12)
                      (event 'd 23)
                      (event 'd 3)
                      (event 'd 13)
                      (event 'd 24)))

  (define spec12 (spec '(a b) 'd '(add1 + - not >) '(t x)
                       '((a (split ((a1 (add1 a)))
                                   (if (> a1 t)
                                       (new-stream-seed ((b (return (+ b d))))      
                                                        a1)))))))
  (check-equal? (interpret-spec spec12 tr3 binding2)
                (list (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (event 'd 12)
                      (event 'd 23)
                      (empty-event)
                      (event 'd 13)
                      (event 'd 24)))
  (define spec14 (spec '(a b) 'd '(add1 + - not >) '(t x)
                       '((a (split ((a1 (add1 a)))
                                   (if (> a1 t)
                                       (new-stream ((b (if-else (undefined? d) (return (add1 (+ a1 b))) (return (+ b d))))))))))))
  (check-equal? (interpret-spec spec14 tr3 binding2)
                (list (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (empty-event)
                      (event 'd 13)
                      (event 'd 24)
                      (empty-event)
                      (event 'd 14)
                      (event 'd 25)))
  )