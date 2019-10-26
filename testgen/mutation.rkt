#lang rosette/safe

(require "sym-trace.rkt"
         "../interpret/interpret-spec/analyzed.rkt"
         "../interpret/interpret-spec/environment.rkt"
         "../interpret/interpret-spec/spec.rkt"
         "../ast/spec.rkt"
         "test-suite.rkt"
         "mut-gen.rkt"
         rackunit
         rosette/lib/match
         rosette/base/struct/struct
         rosette/lib/angelic)

(require racket/engine)

(struct test-suite-gen-result (suite survived killed timeouted not-well-formed) #:transparent)
(define (add-test gen-result test-case)
  (match gen-result
    [(test-suite-gen-result (rx-test-suite test-case-list) survived killed timeouted not-well-formed)
     (test-suite-gen-result (rx-test-suite (cons test-case test-case-list)) survived killed timeouted not-well-formed)]))
(define (add-survived gen-result mutant)
  (struct-copy test-suite-gen-result gen-result
               [survived (cons mutant (test-suite-gen-result-survived gen-result))]))
(define (add-killed gen-result mutant)
  (struct-copy test-suite-gen-result gen-result
               [killed (cons mutant (test-suite-gen-result-killed gen-result))]))
(define (add-timeouted gen-result mutant)
  (struct-copy test-suite-gen-result gen-result
               [timeouted (cons mutant (test-suite-gen-result-timeouted gen-result))]))
(define (add-not-well-formed gen-result mutant)
  (struct-copy test-suite-gen-result gen-result
               [not-well-formed (cons mutant (test-suite-gen-result-not-well-formed gen-result))]))

(define (format-result gen-result)
  (define (format-case test-case)
    (match test-case
      [(rx-test-case trace output)
       (format "[IN:  ~a\n OUT: ~a]" trace output)]))
  (define (format-suite suite)
    (define (iter test-case-list)
      (match test-case-list
        [(list) ""]
        [(list test-case) (format-case test-case)]
        [(cons test-case rest)
         (format "~a\n~a" (format-case test-case) (iter rest))]))
    (match suite
      [(rx-test-suite test-case-list) (iter test-case-list)]))
  (define (format-specs specs)
    (match specs
      [(list) ""]
      [(list spec) (format "~a" spec)]
      [(cons spec rest) (format "~a\n~a" (format "~a" spec) (format-specs (cdr specs)))]))
  (match gen-result
    [(test-suite-gen-result suite survived killed timeouted not-well-formed)
     (format "suite:\n~a\nsurvived:\n~a\nkilled:\n~a\ntimeouted:\n~a\nnot-well-formed:\n~a\n"
             (format-suite suite)
             (format-specs survived)
             (format-specs killed)
             (format-specs timeouted)
             (format-specs not-well-formed))]))

(define (find-distinguish-input spec1 spec2 bindings sym-trace)
  #|(define e (engine (lambda (_)|#
                      (synthesize
                       #:forall (list)
                       #:guarantee
                       (assert (not
                                (equal? (interpret-spec spec1 sym-trace bindings)
                                        (interpret-spec spec2 sym-trace bindings))))))#|))
  (if (engine-run 100000 e)
      (engine-result e)
      'timeout))|#

(define (check-semantics spec-input sym-trace binding-input)
  #|(define e
    (engine
     (lambda (_)|#
       (displayln "Checking")
       (displayln spec-input)
       (let ([val (sat? (time
              (synthesize #:forall (symbolics sym-trace)
                          #:guarantee (assert (interpret-spec spec-input sym-trace binding-input)))))])

         (displayln val)
         val))#|))
  (if (engine-run 20000 e)
      (engine-result e)
      #f))|#

(define (gen-test-on-mutants spec mutants bindings constructor-list start-len max-len step)
  (define (gen-test-on-mutant mutant cur-len)
    (displayln mutant)
    ;semantics should already be checked
    (let* ([sym-trace (get-symbolic-trace constructor-list cur-len)]
           [model (find-distinguish-input spec mutant bindings sym-trace)])
      (if (eq? model 'timeout)
          'timeout
          (if (unsat? model)
              (let ([new-len (+ cur-len step)])
                (if (> new-len max-len)
                    'survived
                    (gen-test-on-mutant mutant new-len)))
              (let* ([trace (evaluate sym-trace (complete-solution model (symbolics sym-trace)))]
                     [output (interpret-spec spec trace bindings)])
                (rx-test-case trace output))))))
  (define max-trace (get-symbolic-trace constructor-list max-len))
  (define trace-5 (get-symbolic-trace constructor-list 5))
  (define-values (well-formed not-well-formed) (partition (lambda (x) (check-semantics x trace-5 bindings)) mutants))
  (define (add-killeds killeds result)
    (match killeds
      [(list) result]
      [(cons killed rest) (add-killed (add-killeds rest result) killed)]))
  (define (iter mutants)
    (displayln (length mutants))
    (match mutants
      
      [(list) (test-suite-gen-result (rx-test-suite '()) '() '() '() not-well-formed)]
      [(cons mutant rest)
       (let ([result (gen-test-on-mutant mutant start-len)])
         (cond [(eq? 'survived result) (add-survived (iter rest) mutant)]
               [(eq? 'timeout result) (add-timeouted (iter rest) mutant)]
               [else
                (let ([killeds (filter (lambda (mutant) (failing-test? (run-case mutant bindings result))) mutants)]
                      [surviveds (filter (lambda (mutant) (success-test? (run-case mutant bindings result))) mutants)])
                  (add-test (add-killeds killeds (iter surviveds)) result))]))]))
  (iter well-formed))

(define (main)
  (define (test-case1)
    (define spec1-input
      (spec '(a b) 'd '(add1 + - not >) '(t x)
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (if-else (undefined? d)
                                                     (return (add1 (+ a1 b)))
                                                     (return (+ b d))))))))))))
    (define spec2-input
      (spec '(a b) 'd '(add1 + - not >) '(t x)
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (if (undefined? d)
                                                (return (add1 (+ a1 b)))
                                                ))))))))))

    (define spec3-input
      (spec '(a b) 'd '(add1 + - not >) '(t x)
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (return (+ b d)))))))))))
    
    (define binding-input (list
                           (binding 'add1 add1)
                           (binding '+ +)
                           (binding '- -)
                           (binding 'not not)
                           (binding 'undefined? undefined?)
                           (binding '> >)
                           (binding 't 1)
                           (binding 'x 1)
                           (binding 'undefined 'undefined)))

    (define constructor-list (list (cons 'a integer-constructor)
                                   (cons 'b integer-constructor)))

    (define sym-trace (get-symbolic-trace
                       constructor-list
                       5))

    (define m (find-distinguish-input spec1-input spec2-input
                                      binding-input sym-trace ))
    (define solved-trace (evaluate sym-trace m))
    (displayln solved-trace)
    (displayln (interpret-spec spec1-input solved-trace binding-input))
    (displayln (interpret-spec spec2-input solved-trace binding-input))

     
    (define m1 (synthesize #:forall (symbolics sym-trace)
                           #:guarantee (assert (interpret-spec spec2-input sym-trace binding-input))))
    (check-equal? (check-semantics spec1-input sym-trace binding-input) #t)
    (check-equal? (check-semantics spec2-input sym-trace binding-input) #t)
    (check-equal? (check-semantics spec3-input sym-trace binding-input) #f)
     
     
    )
  ;(test-case1)


  (define (test-case2)
    (define spec1-input
      (spec '(a b) 'd '(add1 + - not >) '(t x)
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (if-else (undefined? d)
                                                     (return (add1 (+ a1 b)))
                                                     (return (+ b d))))))))))))
    (define spec2-input
      (spec '(a b) 'd '(add1 + - not >) '(t x)
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (if (undefined? d)
                                                (return (add1 (+ a1 b)))
                                                ))))))))))

    (define spec3-input
      (spec '(a b) 'd '(add1 + - not >) '(t x)
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (return (+ b d)))))))))))
    
    (define binding-input (list
                           (binding 'add1 add1)
                           (binding '+ +)
                           (binding '- -)
                           (binding 'not not)
                           (binding 'undefined? undefined?)
                           (binding '> >)
                           (binding 't 1)
                           (binding 'x 1)
                           (binding 'undefined 'undefined)))

    (define constructor-list (list (cons 'a integer-constructor)
                                   (cons 'b integer-constructor)))

    (define result (time (format-result (gen-test-on-mutants spec1-input (list spec2-input spec3-input) binding-input constructor-list 5 20 5))))

    (displayln result)
     
    )
  ;(test-case2)

  
  (define (test-case3)
    (define spec1-input
      (spec '(a b) 'd '(add1 + - not >) '(t x)
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (if-else (undefined? d)
                                                     (return (add1 (+ a1 b)))
                                                     (return (+ b d))))))))))))
    (define mutants (mutate-spec spec1-input 1))
    (displayln (length mutants))
    
    (define binding-input (list
                           (binding 'add1 add1)
                           (binding '+ +)
                           (binding '- -)
                           (binding 'not not)
                           (binding 'undefined? undefined?)
                           (binding '> >)
                           (binding 't 1)
                           (binding 'x 1)
                           (binding 'undefined 'undefined)))

    (define constructor-list (list (cons 'a integer-constructor)
                                   (cons 'b integer-constructor)))


    (define result (time (format-result (gen-test-on-mutants spec1-input mutants binding-input constructor-list 5 20 5))))

    (displayln result)
     
    )
  ;(test-case3)

  (define (test-case4)
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
    (define mutants (mutate-spec drawing-split-spec 2))
    (displayln (length mutants))

    (struct segment (start end) #:transparent)
    (struct point (x y) #:transparent)
    (define (point-constructor)
      (define-symbolic* x integer?)
      (define-symbolic* y integer?)
      (point x y))
    (define (mode-constructor)
      (choose* 'curve-drawing 'curve-ready 'line-drawing 'line-ready))
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

    (define constructor-list (list (cons 'mode mode-constructor)
                                   (cons 'down point-constructor)
                                   (cons 'move point-constructor)))



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

    (displayln (check-semantics drawing-split-spec (get-symbolic-trace constructor-list 5) binding-input))

    (displayln (interpret-spec drawing-split-spec concrete-trace1 binding-input))

    (define result (time (format-result (gen-test-on-mutants drawing-split-spec mutants binding-input constructor-list 5 20 5))))

    (displayln result)
     
    )
  (test-case4)
  )
(main)