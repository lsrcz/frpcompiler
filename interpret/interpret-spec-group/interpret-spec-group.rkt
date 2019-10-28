#lang rosette/safe

(require "environment.rkt")
(require "../../test/test-spec.rkt")
(require "analyzed.rkt")
(require "spec-group-definition.rkt")
(require "../subscribe-fsm.rkt")
(require "../ast/spec.rkt")
(require rosette/lib/match)
(require rosette/base/struct/struct)

(provide interpret-spec-group)

(define (die) (car '()))

(define (analyze body target specidx bitvector-transition)
  (define (analyze-if-imp js-expr branch index indexne)
    (let ([analyzed-js-expr (analyze-js-expr js-expr #t)]
          [analyzed-branch (analyze-inst-imp branch (+ index 1) indexne)])
      (match analyzed-branch
        [(analyzed-value call sons num numne)
         (analyzed-value
          (lambda (env)
            (if (analyzed-js-expr env)
                (call env)
                (environment-glb-env
                 (sub-bv-mask specidx (get-unsub (list-ref bitvector-transition index)) env))))
          sons
          (+ num 1)
          numne)])))

  (define (analyze-if-else-imp js-expr then-branch else-branch index indexne)
    (let ([analyzed-js-expr (analyze-js-expr js-expr #t)]
          [analyzed-then-branch (analyze-inst-imp then-branch index indexne)])
      (match analyzed-then-branch
        [(analyzed-value then-call then-sons then-num then-num-ne)
         (let ([analyzed-else-branch (analyze-inst-imp else-branch (+ then-num index) (+ then-num-ne indexne))])
           (match analyzed-else-branch
             [(analyzed-value else-call else-sons else-num else-num-ne)
              (analyzed-value
               (lambda (env)
                 (if (analyzed-js-expr env)
                     (then-call env)
                     (else-call env)))
               (append then-sons else-sons)
               (+ then-num else-num)
               (+ then-num-ne else-num-ne))]))])))

  (define (analyze-bind-imp name js-expr next index indexne)
    (let ([analyzed-js-expr (analyze-js-expr js-expr #t)]
          [analyzed-next (analyze-inst-imp next index indexne)])
      (match analyzed-next
        [(analyzed-value call sons num numne)
         (analyzed-value
          (lambda (env)
            (let ([value (analyzed-js-expr env)])
              (call (add-const-binding name value env))))
          sons
          num
          numne)])))

  (define (analyze-begin-imp lst index indexne)
    (let* ([let-list (reverse (cdr (reverse lst)))]
           [next (last lst)])
      (let ([let-trans-list
             (map (match-lambda
                    [(list 'let name js-expr)
                     (let ([analyzed-js-expr (analyze-js-expr js-expr #t)]) (binding name analyzed-js-expr))])
                  let-list)]
            [analyzed-next (analyze-inst-imp next index indexne)])
        (match analyzed-next
          [(analyzed-value call sons num numne)
           (define (iter let-trans-list env)
             (if (null? let-trans-list)
                 (call env)
                 (let ([cur (car let-trans-list)]
                       [rest (cdr let-trans-list)])
                   (let ([val ((binding-value cur) env)])
                     (if (too-early? val)
                         (too-early)
                         (iter rest (add-const-binding (binding-name cur) val env)))))))
           (analyzed-value
            (lambda (env)
              (iter let-trans-list env))
            sons
            num
            numne)]))))

  (define (analyze-empty-stream-inst index indexne)
    (analyzed-value
     (lambda (env)
       (environment-glb-env
        (sub-bv-mask
         specidx
         (get-unsub (list-ref bitvector-transition index))
         env)))
     '()
     1
     0))


  (define (analyze-new-stream-inst body index indexne)
    (let ([analyzed (analyze-new-stream body (+ 1 index) (+ 1 indexne))])
      (match analyzed
        [(analyzed-new-stream sons num numne)
         (let ([cur-bvtrans (list-ref bitvector-transition index)])
           (analyzed-value
            (lambda (env)
              (set-ret-value
               target
               'undefined
               (environment-glb-env
                (update-sub-binding
                 specidx
                 indexne
                 (sub-bv-mask
                  (get-unsub cur-bvtrans)
                  (sub-bv-set (get-self cur-bvtrans) env))))
               #t))
            sons
            (+ num 1)
            (+ numne 1)))])))

  (define (analyze-new-stream-initial-inst body initial index indexne)
    (let ([analyzed (analyze-new-stream body (+ 1 index) (+ 1 indexne))]
          [analyzed-js-expr (analyze-js-expr initial)])
      (match analyzed
        [(analyzed-new-stream sons num numne)
         (let ([cur-bvtrans (list-ref bitvector-transition index)])
           (analyzed-value
            (lambda (env)
              (set-ret-value
               target
               (analyzed-js-expr env)
               (environment-glb-env
                (update-sub-binding
                 specidx
                 indexne
                 (sub-bv-mask
                  (get-unsub cur-bvtrans)
                  (sub-bv-set (get-self cur-bvtrans) env))))))
            sons
            (+ num 1)
            (+ numne 1)))])))

  (define (analyze-new-stream-seed-inst body seed index indexne)
    (let ([analyzed (analyze-new-stream body (+ 1 index) (+ 1 indexne))]
          [analyzed-js-expr (analyze-js-expr seed)])
      (match analyzed
        [(analyzed-new-stream sons num numne)
         (let ([cur-bvtrans (list-ref bitvector-transition index)])
           (analyzed-value
            (lambda (env)
              (set-ret-value
               target
               (analyzed-js-expr env)
               (environment-glb-env
                (update-sub-binding
                 specidx
                 indexne
                 (sub-bv-mask
                  (get-unsub cur-bvtrans)
                  (sub-bv-set (get-self cur-bvtrans) env))))
               #t))
            sons
            (+ num 1)
            (+ numne 1)))])))

  (define (analyze-new-stream body index indexne)
    (define (iter body index indexne)
      (match body
        [(list) '()]
        [(cons (list name inst) rest)
         (let ([analyzed-inst (analyze-inst inst index indexne)])
           (cons (list name analyzed-inst)
                 (iter rest (+ index (analyzed-value-processed-num analyzed-inst))
                       (+ indexne (analyzed-value-processed-num-non-empty analyzed-inst)))))]))
    
    (let ([analyzed-body (iter body index indexne)])
      (analyzed-new-stream
       (append (map (match-lambda [(list name (analyzed-value call _ _ _)) (call-def name call (- indexne 1))]) analyzed-body)
               (append-map (match-lambda [(list _ (analyzed-value _ sons _ _)) sons]) analyzed-body))
       (apply + (map (match-lambda [(list _ (analyzed-value _ _ num _)) num]) analyzed-body))
       (apply + (map (match-lambda [(list _ (analyzed-value _ _ _ numne)) numne]) analyzed-body))
       )))

  (define (analyze-inst-imp inst index indexne)
    (match inst
      [(list 'if js-expr branch)
       (analyze-if-imp js-expr branch index indexne)]
      [(list 'if-else js-expr then-branch else-branch)
       (analyze-if-else-imp js-expr then-branch else-branch index indexne)]
      [(list 'bind name js-expr next)
       (analyze-bind-imp name js-expr next index indexne)]
      [(cons 'begin lst)
       (analyze-begin-imp lst index indexne)]
      [(list 'empty-stream)
       (analyze-empty-stream-inst index indexne)]
      [(list 'new-stream body)
       (analyze-new-stream-inst body index indexne)]
      [(list 'new-stream-initial body initial)
       (analyze-new-stream-initial-inst body initial index indexne)]
      [(list 'new-stream-seed body seed)
       (analyze-new-stream-seed-inst body seed index indexne)]))
  ;[_ (error "should not happen")]))

  (define (analyze-js-expr js-expr [only-constant #f])
    (match js-expr
      [(list 'prev _)
       (lambda (env)
         (let ([result (resolve-environment env js-expr target only-constant)])
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
         (let ([result (resolve-environment env js-expr target only-constant)])
           (cond [(resolved? result)
                  (resolved-value result)]
                 [(not-found? result) (die)]
                 [(too-early? result) result])))]))



  (define (analyze-if js-expr branch index indexne)
    (let ([analyzed-js-expr (analyze-js-expr js-expr)]
          [analyzed-branch (analyze-inst branch index indexne)])
      (match analyzed-branch
        [(analyzed-value call sons num numne)
         (analyzed-value
          (lambda (env)
            (let ([js-val (analyzed-js-expr env)])
              (if (too-early? js-val)
                  (too-early)
                  (if js-val
                      (call env)
                      (environment-glb-env env)))))
          sons
          num
          numne)])))

  (define (analyze-if-else js-expr then-branch else-branch index indexne)
    (let ([analyzed-js-expr (analyze-js-expr js-expr)]
          [analyzed-then (analyze-inst then-branch index indexne)])
      (match analyzed-then
        [(analyzed-value then-call then-sons then-num then-numne)
         (let ([analyzed-else (analyze-inst else-branch (+ then-num index) (+ then-numne indexne))])
           (match analyzed-else
             [(analyzed-value else-call else-sons else-num else-numne)
              (analyzed-value
               (lambda (env)
                 (let ([js-val (analyzed-js-expr env)])
                   (if (too-early? js-val)
                       (too-early)
                       (if js-val
                           (then-call env)
                           (else-call env)))))
               (append then-sons else-sons)
               (+ then-num else-num)
               (+ then-numne else-numne))]))])))

  (define (analyze-begin lst index indexne)
    (let* ([let-list (reverse (cdr (reverse lst)))]
           [next (last lst)])
      (let ([let-trans-list
             (map (match-lambda
                    [(list 'let name js-expr)
                     (let ([analyzed-js-expr (analyze-js-expr js-expr)]) (binding name analyzed-js-expr))])
                  let-list)]
            [analyzed-next (analyze-inst next index indexne)])
        (match analyzed-next
          [(analyzed-value call sons num numne)
           (define (iter let-trans-list env)
             (if (null? let-trans-list)
                 (call env)
                 (let ([cur (car let-trans-list)]
                       [rest (cdr let-trans-list)])
                   (let ([val ((binding-value cur) env)])
                     (if (too-early? val)
                         (too-early)
                         (iter rest (add-stream-binding (binding-name cur) val env)))))))
        (analyzed-value
         (lambda (env)
           (iter let-trans-list env))
         sons
         num
         numne)]))))

  (define (analyze-bind name js-expr next index indexne)
    (let ([analyzed-js-expr (analyze-js-expr js-expr)]
          [analyzed-next (analyze-inst next index indexne)])
      (match analyzed-next
        [(analyzed-value call sons num numne)
         (analyzed-value
          (lambda (env)
            (let ([value (analyzed-js-expr env)])
              (if (too-early? value)
                  (too-early)
                  (call (add-stream-binding name value env)))))
          sons
          num
          numne)])))

  (define (analyze-return js-expr index indexne)
    (let ([analyzed-js-expr (analyze-js-expr js-expr)])
      (analyzed-value
       (lambda (env)
         (let ([value (analyzed-js-expr env)])
           (if (too-early? value)
               (too-early)
               (set-ret-value target value (environment-glb-env env)))))
       '()
       0
       0)))

  (define (analyze-split bindings body index indexne)
    (let ([binding-list (map (match-lambda [(list name js-expr) (list name (analyze-js-expr js-expr))]) bindings)]
          [analyzed-body (analyze-inst-imp body index indexne)])
      (match analyzed-body
        [(analyzed-value call sons num numne)
         (define (iter binding-list env)
           (match binding-list
             [(list) (call env)]
             [(list-rest (list name analyzed-js-expr) rest)
              (let ([value (analyzed-js-expr env)])
                (if (too-early? value)
                    (too-early)
                    (iter rest (add-const-binding name (analyzed-js-expr env) env))))]))
         (analyzed-value
          (lambda (env)
            (iter binding-list env))
          sons
          num
          numne)])))

  (define (analyze-inst inst index indexne)
    (match inst
      [(list 'if js-expr branch)
       (analyze-if js-expr branch index indexne)]
      [(list 'if-else js-expr then-branch else-branch)
       (analyze-if-else js-expr then-branch else-branch index indexne)]
      [(list 'return js-expr)
       (analyze-return js-expr index indexne)]
      [(list 'bind name js-expr next)
       (analyze-bind name js-expr next index indexne)]
      [(cons 'begin lst)
       (analyze-begin lst index indexne)]
      [(list 'split bindings body)
       (analyze-split bindings body index indexne)]))
  (match (analyze-new-stream body 1 1)
    [(analyzed-new-stream son num numne)
     (analyzed-new-stream son (+ 1 num) (+ 1 numne))])
  )

(struct analyze-spec-result (analyzed sym-bv-mapping) #:transparent)
(struct analyze-spec-group-result (spec-result-list stream-list) #:transparent)

(define (analyze-spec-group spec-group)
  (define spec-list (spec-group-specs spec-group))
  (define (iter spec-list idx)
    (match spec-list
      [(list) '()]
      [(cons (spec _ output _ _ body) next)
       (let* ([dependency-list (collect-streams body '())]
              [bv-trans (build-bitvector-transition dependency-list)]
              [sym-bv-mapping (build-symbol-bv-mapping dependency-list)]
              [analyzed (analyze body output idx bv-trans)])
       (cons (analyze-spec-result (analyze body output idx bv-trans)
                             sym-bv-mapping)
             (iter next (+ 1 idx))))]))
  (define (collect-inputs spec-list)
    (define (iter spec-list)
      (match spec-list
        [(list) '()]
        [(cons (spec inputs output _ _ _) next)
         (cons output (append inputs (iter next)))]))
    (remove-duplicates (iter spec-list)))
  (analyze-spec-group-result (iter spec-list 0) (collect-inputs spec-list)))

(define (make-list num v)
  (if (= num 0)
      '()
      (cons v (make-list (- num 1) v))))

(define (interpret-spec-group spec-group-input trace bindings)
  (define (build-initial-sub-environment spec-result-list)
    (match spec-result-list
      [(list) '()]
      [(cons (analyze-spec-result analyzed sym-bv-mapping) rest)
       (match analyzed
         [(analyzed-new-stream sons num numne)
          (let ([binding-lst (cons bindings (make-list (- numne 1) #f))])
            (cons
             (sub-environment sons (bv 1 numne) sym-bv-mapping binding-lst)
              (build-initial-sub-environment rest)))])]))
  (match (analyze-spec-group spec-group-input)
    [(analyze-spec-group-result spec-result-list stream-list)
     (define initial-glb-env
       (global-env stream-list
                   (map (lambda (name) (collected-list name '())) stream-list)
                   (map (lambda (name) (collected-with-empty-list name '())) stream-list)
                   (build-initial-sub-environment spec-result-list)))
     (define (run-trace event-lst glb-env)
       (if (too-early? glb-env)
           glb-env
           (match event-lst
             [(list) glb-env]
             [(cons (empty-event) rest)
              (run-trace rest (push-empty-glb glb-env))]
             [(cons (event name value) rest)
              (run-trace rest (set-ret-value name value (push-empty-glb glb-env)))])))
     (run-trace (trace-event-lst trace) initial-glb-env)]))

(module+ test
  (require rackunit)
  (define spec1 (spec '(a d) 'b '(f) '(t) '((a (if d (return (f a)))))))
  (define spec2 (spec '(a d) 'b '(f) '(t) '((a (if-else d (return (f a (prev a))) (return (g a (prev a))))))))
  (define spec3 (spec '(a d) 'b '(f) '(t) '((a (if-else d (return (f a (prev a))) (return a))))))
  (define spec4 (spec '(a d) 'b '(f) '(t) '((a (if-else d (return a) (return (f a (prev a))))))))
  (define spec5 (spec '(a d) 'b '(f) '(t) '((a (if-else (not d) (return a) (return (f a (prev a))))))))
  (define spec6 (spec '(a d) 'b '(f) '(t) '((a (bind t (not d) (if-else t (return a) (return (f a (prev a)))))))))
  (define spec7 (spec '(a d) 'b '(f) '(t) '((a (bind t (not d) (bind x (not (not t)) (if-else x (return a) (return (f a (prev a))))))))))
  (define spec8 (spec '(a d) 'b '(f) '(t) '((a (begin (let t (not d))
                                                      (let x (not (not t)))
                                                      (if-else x (return a) (return (f a (prev a)))))))))

  (define stream-body-for-testing-analyze     '((s1 (split ((sb s1))
                 (if-else sb
                          (new-stream ((s2 (return s2))))
                          (new-stream
                           ((s3 (split ((sb3 s3))
                                       (if-else sb3
                                                (empty-stream)
                                                (if-else sb3
                                                         (new-stream ((s5 (return s5))))
                                                         (new-stream ((s6 (split ((sx6 s6)) (new-stream ((s7 (return s7))))))
                                                                      (sx (split ((sxx sx)) (new-stream ((s8 (return s8)))))))))))))))))))

  (define (check-glb-env glb-env collected-with-empty)
    (check-equal? (length collected-with-empty) (length (global-env-inputs glb-env)))
    (define symbols (global-env-inputs glb-env))
    (define (iter-symbols-check symbols symbols-input)
      (match symbols
        [(list) (void)]
        [(cons cur rest)
         (begin
           (check-true (memq cur symbols-input))
           (iter-symbols-check rest symbols-input))]))
    (iter-symbols-check symbols (map collected-with-empty-list-name collected-with-empty))
    (define (iter-collected-with-empty-check symbols glb-input input)
      (match symbols
        [(list) (void)]
        [(cons cur rest)
         (let* ([find-proc (lambda (x) (eq? cur (collected-with-empty-list-name x)))]
                [glb-val (findf find-proc glb-input)]
                [input-val (findf find-proc glb-input)])
           (check-not-false glb-val)
           (check-not-false input-val)
           (check-equal? glb-val input-val)
           (iter-collected-with-empty-check rest glb-input input))]))
    (iter-collected-with-empty-check symbols (global-env-collected-with-empty glb-env) collected-with-empty)

    
    (define (remove-empty input)
      (match input
        [(collected-with-empty-list name list)
         (collected-list name (map event-value (filter event? list)))]))
    
    (define collected (map remove-empty collected-with-empty))
    (define (iter-collected-check symbols glb-input input)
      (match symbols
        [(list) (void)]
        [(cons cur rest)
         (let* ([find-proc (lambda (x) (eq? cur (collected-list-name x)))]
                [glb-val (findf find-proc glb-input)]
                [input-val (findf find-proc glb-input)])
           (check-not-false glb-val)
           (check-not-false input-val)
           (check-equal? glb-val input-val)
           (iter-collected-check rest glb-input input))]))
    (iter-collected-check symbols (global-env-collected glb-env) collected))
    

  (define (extract-one-symbol-trace symbol trace-input)
    (match trace-input
      [(trace lst)
       (collected-with-empty-list
        symbol
        (filter (lambda (x) (and (event x) (eq? (event-name x) symbol))) lst))]))
  
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
  (define binding1 (list (binding 'f add1) (binding 'g -) (binding 'not not)))
  (check-glb-env
   (interpret-spec-group (spec-group '() (list spec1)) tr1 binding1)
   (append
    (map (lambda (x) (extract-one-symbol-trace x tr1)) '(a d))
    (list (collected-with-empty-list 'b
                                     (list (event 'b 6)
                                           (event 'b 5)
                                           (empty-event)
                                           (empty-event)
                                           (empty-event)
                                           (event 'b 3)
                                           (event 'b 2)
                                           (empty-event))))))
  #|(check-equal? (interpret-spec spec1 tr1 binding1)
                (list
                 (empty-event)
                 (empty-event)
                 (event 'b 3)
                 (empty-event)
                 (empty-event)
                 (empty-event)
                 (event 'b 7)
                 (event 'b 9)))|#
  #|
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
  (displayln (analyze (spec-body spec9) (build-bitvector-transition (collect-streams (spec-body spec9) '()))))
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

  (define drawing-split-spec
      (spec
       '(mode down move)
       'drawing
       '(curve-drawing? undefined? segment append-one list)
       '()
       '((mode
          (split ((mode_snapshot mode)
                  (down_snapshot down))
                 (if-else (curve-drawing? mode_snapshot)
                          (new-stream
                           ((move (if-else (undefined? drawing)
                                           (return (list (segment down_snapshot move)))
                                           (return (append-one drawing (segment (prev move) move)))))))
                          (new-stream-initial () (list))))))))
    (struct segment (start end) #:transparent)
    (struct point (x y) #:transparent)
    (define binding-input (list
                           (binding 'curve-drawing? (lambda (x) (if (point? x)
                                                                    (car '())
                                                                    (eq? x 'curve-drawing)
                                                                    )))
                           (binding 'not not)
                           (binding 'undefined? undefined?)
                           (binding 'list list)
                           (binding 'segment segment)
                           (binding 'append-one (lambda (lst x) (append lst (list x))))))

  (define concrete-trace1
    (trace
     (list
      (event 'down (point 1 2))
      (event 'mode 'curve-drawing)
      (event 'move (point 2 3))
      (event 'move (point 3 4))
      (event 'mode 'curve-ready)
      (event 'move (point 2 3))
      (event 'move (point 3 4)))))

  (displayln (analyze (spec-body drawing-split-spec) (build-bitvector-transition (collect-streams (spec-body drawing-split-spec) '()))))
  
  (displayln (interpret-spec drawing-split-spec concrete-trace1 binding-input)
                )|#
  
  )