#lang rosette/safe

(require "environment.rkt")
(require "../../test/test-spec.rkt")
(require "analyzed.rkt")
(require "../subscribe-fsm.rkt")
(require "../../ast/syntax.rkt")
(require rackunit)
(require rosette/lib/match)

(provide interpret-spec analyze)

(define (die) (car '()))

(define (analyze body defaultval bitvector-transition)
  (define (analyze-if-imp js-expr branch index indexne)
    (let ([analyzed-js-expr (analyze-js-expr js-expr #t)]
          [analyzed-branch (analyze-inst-imp branch (+ index 1) indexne)])
      (match analyzed-branch
        [(analyzed-value call sons num numne)
         (analyzed-value
          (lambda (env)
            (if (analyzed-js-expr env)
                (call env)
                (sub-bv-mask (get-unsub (list-ref bitvector-transition index)) env)))
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
             (map (lambda (x)
                    (let* ([name (cadr x)]
                           [js-expr (caddr x)]
                           [analyzed-js-expr (analyze-js-expr js-expr #t)])
                      (binding name analyzed-js-expr)))
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
                         env
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
       (sub-bv-mask
        (get-unsub (list-ref bitvector-transition index))
        env))
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
              (update-sub-binding
               indexne
               (set-ret-value
                'undefined
                (sub-bv-mask
                 (get-unsub cur-bvtrans)
                 (sub-bv-set (get-self cur-bvtrans) env)))))
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
              (update-sub-binding
               indexne
               (set-ret-value
                (analyzed-js-expr env)
                (sub-bv-mask
                 (get-unsub cur-bvtrans)
                 (sub-bv-set (get-self cur-bvtrans) env)))))
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
              (update-sub-binding
               indexne
               (clear-ret-value
                (set-ret-value
                 (analyzed-js-expr env)
                 (sub-bv-mask
                  (get-unsub cur-bvtrans)
                  (sub-bv-set (get-self cur-bvtrans) env))))))
            sons
            (+ num 1)
            (+ numne 1)))])))

  (define (analyze-new-stream body index indexne)
    (define (iter body index indexne)
      (if (null? body)
          '()
          (let* ([cur (car body)]
                 [rest (cdr body)]
                 [name (car cur)]
                 [inst (cadr cur)])
            (let ([analyzed-inst (analyze-inst inst index indexne)])
              (cons (list name analyzed-inst)
                    (iter rest (+ index (analyzed-value-processed-num analyzed-inst))
                          (+ indexne (analyzed-value-processed-num-non-empty analyzed-inst))))))))
    
    (let ([analyzed-body (iter body index indexne)])
      (analyzed-new-stream
       (append (map
                (lambda (x)
                  (let* ([name (car x)]
                         [analyzed (cadr x)]
                         [call (analyzed-value-call analyzed)])
                    (call-def name call (- indexne 1))))
                analyzed-body)
               (append-map
                (lambda (x)
                  (let* ([analyzed (cadr x)]
                         [sons (analyzed-value-sons analyzed)])
                    sons))
                analyzed-body))
       (apply + (map
                 (lambda (x)
                   (let* ([analyzed (cadr x)]
                          [num (analyzed-value-processed-num analyzed)])
                     num))
                 analyzed-body))
       (apply + (map
                 (lambda (x)
                   (let* ([analyzed (cadr x)]
                          [numne (analyzed-value-processed-num-non-empty analyzed)])
                     numne))
                 analyzed-body))
       )))

  (define (analyze-inst-imp inst index indexne)
    (cond [(if? inst)
           (analyze-if-imp (if-arg inst)
                           (if-branch inst)
                           index
                           indexne)]
          [(if-else? inst)
           (analyze-if-else-imp (if-else-arg inst)
                           (if-else-then-branch inst)
                           (if-else-else-branch inst)
                           index
                           indexne)]
          [(bind? inst)
           (analyze-bind-imp (bind-name inst)
                             (bind-body inst)
                             (bind-inst inst)
                             index
                             indexne)]
          [(begin? inst)
           (analyze-begin-imp (begin-seq inst)
                              index
                              indexne)]
          [(empty-stream? inst)
           (analyze-empty-stream-inst index indexne)]
          [(new-stream? inst)
           (analyze-new-stream-inst (new-stream-body inst)
                                    index
                                    indexne)]
          [(new-stream-initial? inst)
           (analyze-new-stream-initial-inst (new-stream-initial-body inst)
                                            (new-stream-initial-initial inst)
                                            index
                                            indexne)]
          [(new-stream-seed? inst)
           (analyze-new-stream-seed-inst (new-stream-seed-body inst)
                                         (new-stream-seed-seed inst)
                                         index
                                         indexne)]
          [else (die)]))

  (define (analyze-js-expr js-expr [only-constant #f])
    (if (pair? js-expr)
        (if (eq? 'prev (car js-expr))
            (lambda (env)
              (let ([result (resolve-environment env js-expr defaultval only-constant)])
                (cond [(resolved? result)
                       (resolved-value result)]
                      [(not-found? result) (die)]
                      [(too-early? result) result])))
            (let ([f (car js-expr)]
                  [lst (cdr js-expr)])
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
                            (apply analyzed-f-val analyzed-lst-val))))))))
        (lambda (env)
          (let ([result (resolve-environment env js-expr defaultval only-constant)])
            (cond [(resolved? result)
                   (resolved-value result)]
                  [(not-found? result) (die)]
                  [(too-early? result) result])))))

  (define (analyze-if js-expr branch index indexne)
    (let ([analyzed-js-expr (analyze-js-expr js-expr)]
          [analyzed-branch (analyze-inst branch index indexne)])
      (match analyzed-branch
        [(analyzed-value call sons num numne)
         (analyzed-value
          (lambda (env)
            (let ([js-val (analyzed-js-expr env)])
              (if (too-early? js-val)
                  env
                  (if js-val
                      (call env)
                      env))))
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
                       env
                       (if js-val
                           (then-call env)
                           (else-call env)))))
               (append then-sons else-sons)
               (+ then-num else-num)
               (+ then-numne else-numne))]))])))

  (define (iter-not-too-early? js-vals)
    (if (null? js-vals)
        #t
        (let ([cur (car js-vals)]
              [rest (cdr js-vals)])
          (if (too-early? cur)
              #f
              (iter-not-too-early? rest)))))
  
  (define (analyze-if-multi args branch mapping index indexne)
    (let ([analyzed-args (map analyze-js-expr args)]
          [analyzed-mapping (analyze-js-expr mapping)]
          [analyzed-branch (analyze-inst branch index indexne)])
      (match analyzed-branch
        [(analyzed-value call sons num numne)
         (analyzed-value
          (lambda (env)
            (let ([js-vals (map (lambda (x) (x env)) analyzed-args)]
                  )
              (if (and (not (too-early? analyzed-mapping)) (iter-not-too-early? js-vals))
                  (if (apply (analyzed-mapping env) js-vals)
                      (call env)
                      env)
                  env)))
          sons
          num
          numne)])))

  (define (analyze-if-else-multi args then-branch else-branch mapping index indexne)
    (let ([analyzed-args (map analyze-js-expr args)]
          [analyzed-mapping (analyze-js-expr mapping)]
          [analyzed-then (analyze-inst then-branch index indexne)])
      (match analyzed-then
        [(analyzed-value then-call then-sons then-num then-numne)
         (let ([analyzed-else (analyze-inst else-branch (+ then-num index) (+ then-numne indexne))])
           (match analyzed-else
             [(analyzed-value else-call else-sons else-num else-numne)
              (analyzed-value
               (lambda (env)
                 (let ([js-vals (map (lambda (x) (x env)) analyzed-args)])
                   (if (and (not (too-early? analyzed-mapping)) (iter-not-too-early? js-vals))
                       (if (apply (analyzed-mapping env) js-vals)
                           (then-call env)
                           (else-call env))
                       env)))
               (append then-sons else-sons)
               (+ then-num else-num)
               (+ then-numne else-numne))]))])))

  (define (analyze-case-multi args branchs mapping index indexne)
    (define (iter-analyze-branchs branchs index indexne)
      (if (null? branchs)
          '()
          (let ([cur (car branchs)]
                [rest (cdr branchs)])
            (let ([analyzed-cur (analyze-inst cur index indexne)])
              (match analyzed-cur
                [(analyzed-value _ _ num numne)
                 (cons analyzed-cur (iter-analyze-branchs rest (+ num index) (+ indexne numne)))])))))
    (let* ([analyzed-args (map analyze-js-expr args)]
           [analyzed-mapping (analyze-js-expr mapping)]
           [analyzed-branchs (iter-analyze-branchs branchs index indexne)]
           [calls (map analyzed-value-call analyzed-branchs)])
      (analyzed-value
       (lambda (env)
         (let ([js-vals (map (lambda (x) (x env)) analyzed-args)])
           (if (and (not (too-early? analyzed-mapping)) (iter-not-too-early? js-vals))
               (let ([idx (apply (analyzed-mapping env) js-vals)])
                 (if (or (< idx 0) (>= idx (length branchs)))
                     (die)
                     ((list-ref calls idx) env)))
               env)))
       (append* (map analyzed-value-sons analyzed-branchs))
       (apply + (map analyzed-value-processed-num analyzed-branchs))
       (apply + (map analyzed-value-processed-num-non-empty analyzed-branchs)))))

  (define (analyze-begin lst index indexne)
    (let* ([let-list (reverse (cdr (reverse lst)))]
           [next (last lst)])
      (let ([let-trans-list
             (map
              (lambda (x)
                (let* ([name (cadr x)]
                       [js-expr (caddr x)]
                       [analyzed-js-expr (analyze-js-expr js-expr #t)])
                  (binding name analyzed-js-expr)))
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
                         env
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
                  env
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
               env
               (set-ret-value value env))))
       '()
       0
       0)))

  (define (analyze-return-empty index indexne)
    (analyzed-value
     (lambda (env) env)
     '()
     0
     0))

  (define (analyze-split bindings body index indexne)
    (let ([binding-list (map
                         (lambda (x)
                           (let ([name (car x)]
                                 [js-expr (cadr x)])
                             (list name (analyze-js-expr js-expr))))
                         bindings)]
          [analyzed-body (analyze-inst-imp body index indexne)])
      (match analyzed-body
        [(analyzed-value call sons num numne)
         (define (iter binding-list env)
           (if (null? binding-list)
               (call env)
               (let* ([cur (car binding-list)]
                      [rest (cdr binding-list)]
                      [name (car cur)]
                      [analyzed-js-expr (cadr cur)])
                 (let ([value (analyzed-js-expr env)])
                   (if (too-early? value)
                       env
                       (iter rest (add-const-binding name (analyzed-js-expr env) env)))))))
         (analyzed-value
          (lambda (env)
            (iter binding-list env))
          sons
          num
          numne)])))

  (define (analyze-inst inst index indexne)
    (cond [(if? inst)
           (analyze-if (if-arg inst)
                       (if-branch inst)
                       index
                       indexne)]
          [(if-else? inst)
           (analyze-if-else (if-else-arg inst)
                            (if-else-then-branch inst)
                            (if-else-else-branch inst)
                            index
                            indexne)]
          [(if-multi? inst)
           (analyze-if-multi (if-multi-args inst)
                             (if-multi-branch inst)
                             (if-multi-mapping inst)
                             index
                             indexne)]
          [(if-else-multi? inst)
           (analyze-if-else-multi (if-else-multi-args inst)
                                  (if-else-multi-then-branch inst)
                                  (if-else-multi-else-branch inst)
                                  (if-else-multi-mapping inst)
                                  index
                                  indexne)]
          [(case-multi? inst)
           (analyze-case-multi (case-multi-args inst)
                               (case-multi-branchs inst)
                               (case-multi-mapping inst)
                               index
                               indexne)]
          [(return? inst)
           (analyze-return (return-arg inst)
                           index
                           indexne)]
          [(return-empty? inst)
           (analyze-return-empty index indexne)]
          [(bind? inst)
           (analyze-bind (bind-name inst)
                         (bind-body inst)
                         (bind-inst inst)
                         index
                         indexne)]
          [(begin? inst)
           (analyze-begin (begin-seq inst)
                          index
                          indexne)]
          [(split? inst)
           (analyze-split (split-bindings inst)
                          (split-body inst)
                          index
                          indexne)]))
  
  (match (analyze-new-stream body 1 1)
    [(analyzed-new-stream son num numne)
     (analyzed-new-stream son (+ 1 num) (+ 1 numne))])
  )

(define (make-list num v)
  (if (= num 0)
      '()
      (cons v (make-list (- num 1) v))))

(define (interpret-spec spec-input trace bindings)
  (define (construct-list glb-env)
    (match glb-env
      [(global-env trace time inputs output last-value _ sub-list sub-bv sym-bv-mapping sub-binding)
       (if (eq? (length (trace-event-lst trace)) time)
           '()
           (let ([new-glb-env
                  (advance-glb-env
                   (global-env
                    trace time inputs
                    output last-value
                    'undefined sub-list
                    sub-bv sym-bv-mapping
                    sub-binding))])
             (match new-glb-env
               [(global-env _ _ _ output _ return-val _ _ _ _)
                (cons (if (undefined? return-val) (empty-event) (event output return-val)) (construct-list new-glb-env))])))]))
  (match spec-input
    [(spec inputs output funclist constlist defaultval body)
     (let* ([dependency-list (collect-streams body '())]
            [bv-trans (build-bitvector-transition dependency-list)]
            [sym-bv-mapping (build-symbol-bv-mapping dependency-list)]
            [analyzed (analyze body defaultval bv-trans)])
       (match analyzed
         [(analyzed-new-stream sons num numne)
          (let* ([binding-lst (cons bindings (make-list (- numne 1) #f))]
                 [glb-env (global-env trace 0 inputs output 'undefined 'undefined sons (bv 1 numne) sym-bv-mapping binding-lst)])
            (construct-list glb-env))]))]))

#;(define (main)
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

  (displayln (analyze (spec-body spec1) (build-bitvector-transition (collect-streams (spec-body spec1) '()))))

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

  (displayln (analyze stream-body-for-testing-analyze (build-bitvector-transition (collect-streams stream-body-for-testing-analyze '()))))

  
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
                )

  (define if-multi-spec
    (spec
     '(a)
     'b
     '(f =)
     '(one two)
     '((a (if-multi ((= a one) (= a two)) (return a) f)))))
  (define if-multi-trace
    (trace
     (list
      (event 'a 0)
      (event 'a 1)
      (event 'a 2)
      (event 'a 3))))
  (define if-multi-binding
    (list
     (binding 'f (lambda (x y) (and (not x) (not y))))
     (binding '= =)
     (binding 'one 1)
     (binding 'two 2)))

  (displayln (interpret-spec if-multi-spec if-multi-trace if-multi-binding))
  
  
  )