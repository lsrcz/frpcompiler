#lang rosette/safe

(require rackunit)
(require "analyzed.rkt")
(require rosette/lib/match)
(require rosette/base/struct/struct)

(provide (all-defined-out))

(define (undefined? x) (eq? x 'undefined))

(struct binding (name value) #:transparent)
(struct sub (bindings body) #:transparent)

(struct global-env (trace time inputs output last-value return-val active-sub) #:transparent)
(struct local-env (stream-bindings const-bindings) #:transparent)
(struct environment (glb-env loc-env) #:transparent)
(struct resolved (value) #:transparent)
(struct not-found () #:transparent)
(struct too-early () #:transparent)


(struct event (name value) #:transparent)
(struct empty-event () #:transparent)
(struct trace (event-lst) #:transparent)

(define (get-event-by-time trace time)
  (list-ref (trace-event-lst trace) time))
(define (get-value trace time name prev-num)
  (define (iter num remaining)
    (match remaining
      [(list) (too-early)]
      [(cons cur rest)
       (let ([name-eq (and (not (empty-event? cur)) (eq? (event-name cur) name))]
             [num-zero (= num 0)])
         (if name-eq
             (if num-zero
                 (resolved (event-value cur))
                 (iter (- num 1) rest))
             (iter num rest)))]))
  (let* ([remaining (reverse (take (trace-event-lst trace) (+ 1 time)))])
    (iter prev-num remaining)))

(define (resolve-environment env sym [only-constant #f])
  (define (resolve-input trace sym time)
    (define (iter sym num)
      (match sym
        [(list 'prev sym1) (iter sym1 (+ 1 num))]
        [_ (get-value trace time sym num)]))
    (iter sym 0))
  (define (resolve-list lst sym)
    (let ([filtered (filter (match-lambda [(binding name value) (eq? name sym)]) lst)])
      (if (null? filtered)
          (not-found)
          (resolved (binding-value (car filtered))))))
  (match env
    [(environment (global-env trace time inputs output last-value _ _)
                  (local-env stream-bindings const-bindings))
     (if only-constant
         (resolve-list (append const-bindings) sym)
         (if (list? sym)
             (resolve-input trace sym time)
             (let ([resolved-binding (resolve-list (append const-bindings stream-bindings) sym)])
               (if (resolved? resolved-binding)
                   resolved-binding
                   (if (eq? sym output)
                       (resolved last-value)
                       (resolve-input trace sym time))))))]))

(define (get-event env)
  (match env
    [(environment (global-env trace time _ _ _ _ _) _)
     (get-event-by-time trace time)]))

(define (update-loc-env env new-loc-env)
  (struct-copy environment env [loc-env new-loc-env]))
(define (update-glb-env env new-glb-env)
  (struct-copy environment env [glb-env new-glb-env]))

(define (add-const-binding name value env)
  (update-loc-env
   env
   (let ([loc-env (environment-loc-env env)])
     (struct-copy local-env loc-env
                  [const-bindings
                   (cons (binding name value)
                         (local-env-const-bindings loc-env))]))))

(define (advance-time-glb glb-env)
  (struct-copy global-env glb-env [time (+ (global-env-time glb-env) 1)]))

(define (advance-time env)
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)])
     (advance-time-glb glb-env))))

(define (add-stream-binding name value env)
  (update-loc-env
   env
   (let ([loc-env (environment-loc-env env)])
     (struct-copy local-env loc-env
                  [stream-bindings
                   (cons (binding name value)
                         (local-env-stream-bindings loc-env))]))))

(define (set-ret-value value env)
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)])
     (struct-copy global-env glb-env
                  [last-value value]
                  [return-val value]
                  ))))

(define (clear-ret-value env)
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)])
     (struct-copy global-env glb-env [return-val 'undefined]))))

(define (is-subscribed? stream-body glb-env)
  (if (memq stream-body (map sub-body (global-env-active-sub glb-env)))
      #t
      #f))

(define (subscribe stream-body env)
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)]
         [loc-env (environment-loc-env env)])
     (if (is-subscribed? stream-body glb-env)
         glb-env
         (struct-copy global-env glb-env [active-sub
                                          (cons (sub (local-env-const-bindings loc-env) stream-body)
                                                (global-env-active-sub glb-env))])))))

(define (unsubscribe stream-body env)
  (define (remove-sub active-sub)
    (match active-sub
      [(list) '()]
      [(cons cur rest)
       (match cur
         [(sub _ body)
          (if (eq? body stream-body)
              rest
              (cons cur (remove-sub rest)))])]))
  ;(if (is-subscribed? stream-body (environment-glb-env env))
      (let ([unsubed-env ((analyzed-value-unsub stream-body) env)])
        (update-glb-env
         unsubed-env
         (let ([glb-env (environment-glb-env unsubed-env)])
           (struct-copy global-env glb-env [active-sub
                                            (remove-sub (global-env-active-sub glb-env))])))))
      ;env))


(define (main-env)
  (define tr
    (trace
     (list
      (event 'a 1)
      (empty-event)
      (event 'a 2)
      (event 'b 10)
      (empty-event)
      (event 'b 11)
      (event 'a 3)
      (event 'b 12))))
  (check-equal? (get-value tr 0 'a 0) (resolved 1))
  (check-equal? (get-value tr 0 'a 1) (not-found))
  (check-equal? (get-value tr 1 'a 0) (resolved 1))
  (check-equal? (get-value tr 1 'a 1) (not-found))
  (check-equal? (get-value tr 2 'a 0) (resolved 2))
  (check-equal? (get-value tr 2 'a 1) (resolved 1))
  (check-equal? (get-value tr 3 'a 0) (resolved 2))
  (check-equal? (get-value tr 3 'a 1) (resolved 1))
  (check-equal? (get-value tr 4 'a 0) (resolved 2))
  (check-equal? (get-value tr 4 'a 1) (resolved 1))
  (check-equal? (get-value tr 5 'a 0) (resolved 2))
  (check-equal? (get-value tr 5 'a 1) (resolved 1))
  (check-equal? (get-value tr 6 'a 0) (resolved 3))
  (check-equal? (get-value tr 6 'a 1) (resolved 2))
  
  (check-equal? (get-value tr 0 'b 0) (not-found))
  (check-equal? (get-value tr 1 'b 0) (not-found))
  (check-equal? (get-value tr 2 'b 0) (not-found))
  (check-equal? (get-value tr 3 'b 0) (resolved 10))
  (check-equal? (get-value tr 3 'b 1) (not-found))
  (check-equal? (get-value tr 5 'b 0) (resolved 11))
  
  (check-equal? (get-value tr 5 'b 1) (resolved 10))
  (check-equal? (get-value tr 6 'b 0) (resolved 11))
  (check-equal? (get-value tr 6 'b 1) (resolved 10))

  (define glb-env
    (global-env
     (trace
      (list
       (event 'a 1)
       (empty-event)
       (event 'a 2)
       (event 'b 10)
       (empty-event)
       (event 'b 11)
       (event 'a 3)
       (event 'b 12)))
     0
     '(a b)
     'x
     -100
     0
     '())
  )
  (define loc-env
    (local-env
     (list
      (binding 'c -10)
      (binding 'd -11))
     (list
      (binding 'c -1)
      (binding 'e -2))))
  (define env (environment glb-env loc-env))
  (check-equal? (resolve-environment env 'a) (resolved 1))
  (check-equal? (resolve-environment env '(prev a)) (not-found))
  (check-equal? (resolve-environment (advance-time (advance-time env)) '(prev a)) (resolved 1))
  (check-equal? (resolve-environment (advance-time (advance-time env)) 'c) (resolved -1))
  (check-equal? (resolve-environment (advance-time (advance-time env)) 'd) (resolved -11))
  (check-equal? (resolve-environment (advance-time (advance-time env)) 'x) (resolved -100))
  )
