#lang rosette/safe

(require "../test/trace.rkt"
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

(define (find-distinguish-input interpreter spec1 spec2 sym-trace)
  #|(define e (engine (lambda (_)|#
                      (synthesize
                       #:forall (list)
                       #:guarantee
                       (assert (not
                                (equal? (interpreter spec1 sym-trace)
                                        (interpreter spec2 sym-trace))))))#|))
  (if (engine-run 100000 e)
      (engine-result e)
      'timeout))|#

(define (check-semantics interpreter spec-input sym-trace)
  #|(define e
    (engine
     (lambda (_)|#
       (displayln "Checking")
       (displayln spec-input)
       (let ([val (sat? (time
              (synthesize #:forall (symbolics sym-trace)
                          #:guarantee (assert (interpreter spec-input sym-trace)))))])

         (displayln val)
         val))#|))
  (if (engine-run 20000 e)
      (engine-result e)
      #f))|#

(define (gen-test-on-mutants spec mutants interpreter constructor-list start-len max-len step)
  (define (gen-test-on-mutant mutant cur-len)
    (displayln mutant)
    ;semantics should already be checked
    (let* ([sym-trace (get-symbolic-trace constructor-list cur-len)]
           [model (find-distinguish-input interpreter spec mutant sym-trace)])
      (if (eq? model 'timeout)
          'timeout
          (if (unsat? model)
              (let ([new-len (+ cur-len step)])
                (if (> new-len max-len)
                    'survived
                    (gen-test-on-mutant mutant new-len)))
              (let* ([trace (evaluate sym-trace (complete-solution model (symbolics sym-trace)))]
                     [output (interpreter spec trace)])
                (rx-test-case trace output))))))
  (define max-trace (get-symbolic-trace constructor-list max-len))
  (define (partition f lst)
    (if (null? lst)
        (cons '() '())
        (let* ([t (partition f (cdr lst))]
               [x (car t)]
               [y (cdr t)])
          (if (f (car lst))
              (cons (cons (car lst) x) y)
              (cons x (cons (car lst) y))))))
  (define partitioned (partition (lambda (x) (check-semantics interpreter x max-trace)) mutants))
  (define well-formed (car partitioned))
  (define not-well-formed (cdr partitioned))

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
                (let ([killeds (filter (lambda (mutant) (failing-test? (run-case interpreter mutant result))) mutants)]
                      [surviveds (filter (lambda (mutant) (success-test? (run-case interpreter mutant result))) mutants)])
                  (add-test (add-killeds killeds (iter surviveds)) result))]))]))
  (iter well-formed))

(module+ test
  (define (test-case1)
    (define spec1-input
      (spec '(a b) 'd '(add1 + - not >) '(t x) '()
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (if-else (undefined? d)
                                                     (return (add1 (+ a1 b)))
                                                     (return (+ b d))))))))))))
    (define spec2-input
      (spec '(a b) 'd '(add1 + - not >) '(t x) '()
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (if (undefined? d)
                                                (return (add1 (+ a1 b)))
                                                ))))))))))

    (define spec3-input
      (spec '(a b) 'd '(add1 + - not >) '(t x) '()
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
    (define (interpreter spec-input trace) (interpret-spec spec-input trace binding-input))

    (define constructor-list (list (cons 'a integer-constructor)
                                   (cons 'b integer-constructor)))

    (define sym-trace (get-symbolic-trace
                       constructor-list
                       5))

    (define m (find-distinguish-input interpreter spec1-input spec2-input sym-trace))
    (define solved-trace (evaluate sym-trace m))
    (displayln solved-trace)
    (displayln (interpreter spec1-input solved-trace))
    (displayln (interpreter spec2-input solved-trace))

     
    (define m1 (synthesize #:forall (symbolics sym-trace)
                           #:guarantee (assert (interpreter spec2-input sym-trace))))
    (check-equal? (check-semantics interpreter spec1-input sym-trace) #t)
    (check-equal? (check-semantics interpreter spec2-input sym-trace) #t)
    (check-equal? (check-semantics interpreter spec3-input sym-trace) #f)
    )



  (define (test-case2)
    (define spec1-input
      (spec '(a b) 'd '(add1 + - not >) '(t x) '()
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (if-else (undefined? d)
                                                     (return (add1 (+ a1 b)))
                                                     (return (+ b d))))))))))))
    (define spec2-input
      (spec '(a b) 'd '(add1 + - not >) '(t x) '()
            '((a (split ((a1 (add1 a)))
                        (if (> a1 t)
                            (new-stream ((b (if (undefined? d)
                                                (return (add1 (+ a1 b)))
                                                ))))))))))

    (define spec3-input
      (spec '(a b) 'd '(add1 + - not >) '(t x) '()
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

    (define (interpreter spec-input trace) (interpret-spec spec-input trace binding-input))

    (define constructor-list (list (cons 'a integer-constructor)
                                   (cons 'b integer-constructor)))

    (define result (time (format-result (gen-test-on-mutants spec1-input (list spec2-input spec3-input) interpreter constructor-list 1 5 1))))

    (displayln result)
     
    )
  ;(test-case2)

  
  (define (test-case3)
    (define spec1-input
      (spec '(a b) 'd '(add1 + - not >) '(t x) '()
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

    (define (interpreter spec-input trace) (interpret-spec spec-input trace binding-input))

    (define constructor-list (list (cons 'a integer-constructor)
                                   (cons 'b integer-constructor)))


    (define result (time (format-result (gen-test-on-mutants spec1-input mutants interpreter constructor-list 1 5 1))))

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
    (define (interpreter spec trace) (interpret-spec spec trace binding-input))

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

    (displayln (check-semantics interpreter drawing-split-spec (get-symbolic-trace constructor-list 5)))

    (displayln (interpreter drawing-split-spec concrete-trace1))

    (define result (time (format-result (gen-test-on-mutants drawing-split-spec mutants interpreter constructor-list 1 5 1))))

    (displayln result)
     
    )

  (define (test-case5)
    (define sprinkler-spec
      (spec
       '(clock motion)
       'sprinkler
       '(eq? undefined?)
       '(six sixten detected not-detected on off f1 f2)
       '((clock six) (motion not-detected))
       '((clock (case-multi ((eq? clock six)
                             (eq? clock sixten)
                             (and (eq? motion detected) (eq? (prev motion) not-detected))
                             (and (eq? motion not-detected) (eq? (prev motion) detected)))
                            ((return on)
                             (return off)
                             (return-empty))
                            f1))
         (motion (case-multi ((eq? clock six)
                              (eq? clock sixten)
                              (and (eq? motion detected) (eq? (prev motion) not-detected))
                              (and (eq? motion not-detected) (eq? (prev motion) detected)))
                             ((return on)
                              (return off)
                              (return-empty))
                             f2)))))

    (define mutants (mutate-spec sprinkler-spec 2))
    (displayln (length mutants))

    (define (clock-constructor) (choose* 'six 'sixten))
    (define (motion-constructor) (choose* 'detected 'not-detected))

    
    (define (f1 b1 b2 b3 b4)
      (cond [b1 0]
            [b2 1]
            [else 2]))
    (define (f2 b1 b2 b3 b4)
      (cond [(and b4 b1) 0]
            [b3 1]
            [else 2]))

(define six 'six)
(define sixten 'sixten)
(define detected 'detected)
(define not-detected 'not-detected)
(define on 'on)
(define off 'off)
    
(define (and-func a b)
  (and a b))



    (define binding-input (list
                           (binding 'f1 f1)
                           (binding 'f2 f2)
                           (binding 'eq? eq?)
                           (binding 'six six)
                           (binding 'sixten sixten)
                           (binding 'detected detected)
                           (binding 'not-detected not-detected)
                           (binding 'undefined? undefined?)
                           (binding 'not not)
                           (binding 'and and-func)
                           (binding 'on on)
                           (binding 'off off)))

    (define (interpreter spec trace) (interpret-spec spec trace binding-input))

    (define constructor-list (list
                          (cons 'clock clock-constructor)
                          (cons 'motion motion-constructor)))

    (displayln (check-semantics interpreter sprinkler-spec (get-symbolic-trace constructor-list 5)))

    (define result (time (format-result (gen-test-on-mutants sprinkler-spec mutants interpreter constructor-list 1 5 1))))

    (displayln result)
     
    )
  ;(test-case4)
    (test-case5)
  )
