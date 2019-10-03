#lang rosette/safe

(require "analyzed.rkt")
(require rackunit)
(require rosette/lib/match)
(require rosette/base/struct/struct)

(provide (all-defined-out))

(define (undefined? x) (eq? x 'undefined))

(struct binding (name value) #:transparent)
(struct sub (to bindings body) #:transparent)
(struct call-def (name call index) #:transparent)

(struct sub-environment (sub-list sub-bv sym-bv-mapping sub-binding) #:transparent)

(struct collected-list (name value-lst) #:transparent)
(struct collected-with-empty-list (name event-lst) #:transparent)
(struct global-env (inputs collected collected-with-empty sub-env-list) #:transparent)
(struct local-env (stream-bindings const-bindings) #:transparent)
(struct environment (glb-env loc-env) #:transparent)
(struct resolved (value) #:transparent)
(struct not-found () #:transparent)
(struct too-early () #:transparent)

(struct event (name value) #:transparent)
(struct empty-event () #:transparent)
(struct trace (event-lst) #:transparent)

(struct pseudo-value (value) #:transparent)

(define (get-value-collected collected name prev-num target)
  (define (iter num remaining)
    (match remaining
      [(list)
       (if (eq? name target)
           (resolved 'undefined)
           (too-early))]
      [(cons cur rest)
       (if (and (pseudo-value? cur) (not (eq? name target)))
           (iter name rest)
           (if (= num 0)
               (if (pseudo-value? cur)
                   (resolved (pseudo-value-value cur))
                   (resolved cur))
               (iter (- num 1) rest)))]))
  (define (find-name collected)
    (match collected
      [(list) (not-found)]
      [(cons (collected-list cname value-lst) rest)
       (if (eq? cname name)
           (iter prev-num value-lst)
           (find-name rest))]))
  (find-name collected))

(define (resolve-environment env sym target [only-constant #f])
  (define (resolve-input sym collected)
    (define (iter sym num)
      (match sym
        [(list 'prev sym1) (iter sym1 (+ 1 num))]
        [_ (get-value-collected collected sym num target)]))
    (iter sym 0))
  (define (resolve-list lst sym)
    (let ([filtered (filter (match-lambda [(binding name value) (eq? name sym)]) lst)])
      (if (null? filtered)
          (not-found)
          (resolved (binding-value (car filtered))))))
  (match env
    [(environment (global-env _ collected _ _)
                  (local-env stream-bindings const-bindings))
     (if only-constant
         (resolve-list (append const-bindings) sym)
         (if (list? sym)
             (resolve-input sym collected)
             (let ([resolved-binding
                    (resolve-list (append const-bindings stream-bindings) sym)])
               (if (resolved? resolved-binding)
                   resolved-binding
                   (resolve-input sym collected)))))]))
         

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

(define (add-stream-binding name value env)
  (update-loc-env
   env
   (let ([loc-env (environment-loc-env env)])
     (struct-copy local-env loc-env
                  [stream-bindings
                   (cons (binding name value)
                         (local-env-stream-bindings loc-env))]))))

(define (push-empty-glb glb-env)
  (struct-copy global-env glb-env
               [collected-with-empty
                (map (match-lambda [(collected-with-empty-list name event-lst)
                                    (collected-with-empty-list name (cons (empty-event) event-lst))])
                     (global-env-collected-with-empty glb-env))]))
   

(define (set-ret-value target value glb-env [pseudo-value #f])
  (define (update-collected name value collected)
    (match collected
      [(list) (void)] ; should not happen
      [(cons cur rest)
       (match cur
         [(collected-list name1 value-lst)
          (if (eq? name1 name)
              (cons
               (collected-list name (cons value value-lst)) rest)
              (cons cur (update-collected name value rest)))])]))
  (define (update-collected-with-empty name value collected)
    (match collected
      [(list) (void)]
      [(cons cur rest)
       (match cur
         [(collected-with-empty-list name1 event-lst)
          (if (eq? name1 name)
              (cons
               (collected-with-empty-list name (cons (event name value) (cdr event-lst)))
               rest)
              (cons cur (update-collected-with-empty name value rest)))])]))
  (match glb-env
    [;(environment
      (global-env inputs collected collected-with-empty sub-env-list)
      ;(local-env _ _))
     (if pseudo-value
         (global-env inputs
                     (update-collected target (pseudo-value value) collected)
                     collected-with-empty
                     sub-env-list)
         (run-on-env target value
                     (global-env inputs
                                 (update-collected target value collected)
                                 (update-collected-with-empty target value collected-with-empty)
                                 sub-env-list)))]))

(define (run-on-env from value glb-env)
  (define (is-subscribed? name sub-env)
    (match sub-env
      [(sub-environment sub-list sub-bv sym-bv-mapping sub-binding)
       (let ([assoc-val (assoc name sym-bv-mapping)])
         (if assoc-val
             (not (bveq (bv 0 (length sub-binding)) (bvand sub-bv (cdr assoc-val))))
             #f))]))
  (define (get-subscription name sub-list)
    (match sub-list
      [(list) 'no]
      [(cons cur rest)
       (match cur
         [(call-def namec _ _)
          (if (eq? namec name)
              cur
              (get-subscription name rest))])]))
  (define (iter-on-sub-env-list sub-env-list glb-env)
    (if (too-early? glb-env)
        glb-env
        (match sub-env-list
          [(list) glb-env]
          [(cons sub-env rest)
           (iter-on-sub-env-list rest
                                 (if (is-subscribed? from sub-env)
                                     (match (get-subscription from (sub-environment-sub-list sub-env))
                                       [(call-def _ call idx)
                                        (let ([env (environment glb-env (local-env '() (list-ref (sub-environment-sub-binding sub-env) idx)))])
                                          (call env))])
                                     glb-env))])))
  (match glb-env
    [(global-env _ _ _ sub-env-list) (iter-on-sub-env-list sub-env-list glb-env)]))

(define (list-update v idx lst)
  (if (= idx 0)
      (cons v (cdr lst))
      (cons (car lst) (list-update v (- idx 1) (cdr lst)))))

(define (sub-bv-mask specidx value env)
  (define (sub-env-mask idx value sub-env-list)
    (if (= idx 0)
        (match sub-env-list
          [(cons cur rest)
           (cons
            (struct-copy sub-environment cur
                         [sub-bv (bvand value (sub-environment-sub-bv cur))])
            rest)])
        (cons (car sub-env-list) (sub-env-mask (- idx 1) value (cdr sub-env-list)))))
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)])
     (struct-copy
      global-env glb-env
      [sub-env-list
       (sub-env-mask specidx value
                     (global-env-sub-env-list glb-env))]))))

(define (sub-bv-set specidx value env)
  (define (sub-env-set idx value sub-env-list)
    (if (= idx 0)
        (match sub-env-list
          [(cons cur rest)
           (cons
            (struct-copy sub-environment cur
                         [sub-bv (bvor value (sub-environment-sub-bv cur))])
            rest)])
        (cons (car sub-env-list) (sub-env-set (= idx 1) value (cdr sub-env-list)))))
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)])
     (struct-copy
      global-env glb-env
      [sub-env-list
       (sub-env-set specidx value
                    (global-env-sub-env-list glb-env))]))))

(define (update-sub-binding specidx streamidx env)
  (define (sub-env-update specidx sub-env-list)
    (if (= specidx 0)
        (match sub-env-list
          [(cons cur rest)
           (cons (struct-copy sub-environment cur
                              [sub-binding
                               (list-update (local-env-const-bindings (environment-loc-env env)) streamidx
                                            (sub-environment-sub-binding cur))])
                 rest)])
        (cons (car sub-env-list) (sub-env-update (- specidx 1) (cdr sub-env-list)))))
  (update-glb-env
   env
   (let ([glb-env (environment-glb-env env)]
         [loc-env (environment-loc-env env)])
     (struct-copy global-env glb-env
                  [sub-env-list
                   (sub-env-update specidx (global-env-sub-env-list glb-env))]))))