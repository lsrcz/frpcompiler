#lang rosette/safe

;(require rackunit)
(require "analyzed.rkt"
         "../../test/trace.rkt")
(require rosette/lib/match)

(provide (all-defined-out))

(define (undefined? x) (eq? x 'undefined))

(struct binding (name value) #:transparent)
(struct sub (bindings body) #:transparent)
(struct call-def (name call index) #:transparent)

(struct global-env (trace time inputs output last-value return-val
                    sub-list sub-bv sym-bv-mapping sub-binding) #:transparent)
(struct local-env (stream-bindings const-bindings) #:transparent)
(struct environment (glb-env loc-env) #:transparent)
(struct resolved (value) #:transparent)
(struct not-found () #:transparent)
(struct too-early () #:transparent)

(define (get-event-by-time trace time)
  (list-ref (trace-event-lst trace) time))
(define (get-value trace time name prev-num)
  (define (iter num remaining)
    (if (null? remaining)
        (too-early)
        (let ([cur (car remaining)]
              [rest (cdr remaining)])
          (let ([name-eq (and (not (empty-event? cur)) (eq? (event-name cur) name))]
                [num-zero (= num 0)])
            (if name-eq
                (if num-zero
                    (resolved (event-value cur))
                    (iter (- num 1) rest))
                (iter num rest))))))
  (let* ([remaining (reverse (take (trace-event-lst trace) (+ 1 time)))])
    (iter prev-num remaining)))

(define (resolve-environment env sym defaultval [only-constant #f])
  (define (resolve-input trace sym time)
    (define (iter sym num)
      (if (pair? sym)
          (iter (cadr sym) (+ 1 num))
          (let ([val (get-value trace time sym num)])
            (if (too-early? val)
                (let ([default (assoc sym defaultval)])
                  (if default
                      (resolve-environment env (cadr default) defaultval #t)
                      (too-early)))
                val))))
    (iter sym 0))
  (define (resolve-list lst sym)
    (if (null? lst)
        (not-found)
        (let* ([cur (car lst)]
               [rest (cdr lst)]
               [name (binding-name cur)]
               [value (binding-value cur)])
          (if (eq? name sym)
              (resolved value)
              (resolve-list rest sym)))))
  (match env
    [(environment (global-env trace time inputs output last-value _ _ _ _ _)
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
    [(environment (global-env trace time _ _ _ _ _ _ _ _) _)
     (get-event-by-time trace time)]))

(define (update-loc-env env new-loc-env)
  (match env
    [(environment glb-env loc-env) (environment glb-env new-loc-env)]))
(define (update-glb-env env new-glb-env)
  (match env
    [(environment glb-env loc-env) (environment new-glb-env loc-env)]))

(define (add-const-binding name value env)
  (update-loc-env
   env
   (let ([loc-env (environment-loc-env env)])
     (match loc-env
       [(local-env stream-bindings const-bindings)
        (local-env stream-bindings
                   (cons (binding name value) const-bindings))]))))

(define (advance-time-glb glb-env)
  (match glb-env
    [(global-env trace time inputs output last-value return-val
                 sub-list sub-bv sym-bv-mapping sub-binding)
     (global-env trace (+ time 1) inputs output last-value return-val
                 sub-list sub-bv sym-bv-mapping sub-binding)]))

(define (advance-time env)
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)])
     (advance-time-glb glb-env))))

(define (add-stream-binding name value env)
  (update-loc-env
   env
   (let ([loc-env (environment-loc-env env)])
     (match loc-env
       [(local-env stream-bindings const-bindings)
        (local-env (cons (binding name value) stream-bindings)
                   const-bindings)]))))

(define (set-ret-value value env)
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)])
   (match glb-env
    [(global-env trace time inputs output last-value return-val
                 sub-list sub-bv sym-bv-mapping sub-binding)
     (global-env trace time inputs output value value
                 sub-list sub-bv sym-bv-mapping sub-binding)]))))

(define (clear-ret-value env)
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)])
   (match glb-env
    [(global-env trace time inputs output last-value return-val
                 sub-list sub-bv sym-bv-mapping sub-binding)
     (global-env trace time inputs output last-value 'undefined
                 sub-list sub-bv sym-bv-mapping sub-binding)]))))

(define (sub-bv-mask value env)
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)])
   (match glb-env
    [(global-env trace time inputs output last-value return-val
                 sub-list sub-bv sym-bv-mapping sub-binding)
     (global-env trace time inputs output last-value return-val
                 sub-list (bvand value sub-bv) sym-bv-mapping sub-binding)]))))

(define (sub-bv-set value env)
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)])
   (match glb-env
    [(global-env trace time inputs output last-value return-val
                 sub-list sub-bv sym-bv-mapping sub-binding)
     (global-env trace time inputs output last-value return-val
                 sub-list (bvor value sub-bv) sym-bv-mapping sub-binding)]))))

(define (list-update v idx lst)
  (if (= idx 0)
      (cons v (cdr lst))
      (cons (car lst) (list-update v (- idx 1) (cdr lst)))))

(define (update-sub-binding index env)
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)]
         [loc-env (environment-loc-env env)])
   (match glb-env
    [(global-env trace time inputs output last-value return-val
                 sub-list sub-bv sym-bv-mapping sub-binding)
     (global-env trace time inputs output last-value return-val
                 sub-list sub-bv sym-bv-mapping
                 (list-update (local-env-const-bindings loc-env) index
                              sub-binding))]))))

(define (is-subscribed? name glb-env)
  (match glb-env
    [(global-env _ _ _ _ _ _ sub-list sub-bv sym-bv-mapping sub-binding)
     (let ([assoc-val (assoc name sym-bv-mapping)])
       (if assoc-val
           (not (bveq (bv 0 (length sub-binding)) (bvand sub-bv (cdr assoc-val))))
           #f))]
    ))

(define (advance-glb-env glb-env)
  (define (get-sub name sub-list)
    (if (null? sub-list)
        'no
        (let ([cur (car sub-list)]
              [rest (cdr sub-list)])
          (match cur
            [(call-def namec _ _)
             (if (eq? namec name)
                 cur
                 (get-sub name rest))]))))
  (match glb-env
    [(global-env trace time _ _ _ _ sub-list _ _ sub-binding)
     (let ([evt (get-event-by-time trace time)])
       (advance-time-glb
        (if (or (empty-event? evt)
                (not (is-subscribed? (event-name evt) glb-env)))
            glb-env
            (let ([sub (get-sub (event-name evt) sub-list)])
              (match sub
                [(call-def _ call idx)
                 (let ([env (environment glb-env (local-env '() (list-ref sub-binding idx)))])
                   (environment-glb-env (call env)))])))))]))
              
(module+ test
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
     '()
     (bv 1 2)
     '()
     )
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
