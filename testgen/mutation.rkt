#lang rosette/safe

(require "sym-trace.rkt"
         "../interpret/analyzed.rkt"
         "../interpret/environment.rkt"
         "../interpret/spec.rkt"
         "../test/test-spec.rkt"
         "test-suite.rkt"
         rackunit
         rosette/lib/match
         rosette/base/struct/struct)

(struct test-suite-gen-result (suite survived killed not-well-formed) #:transparent)
(define (add-test gen-result test-case)
  (match gen-result
    [(test-suite-gen-result (rx-test-suite test-case-list) survived killed not-well-formed)
     (test-suite-gen-result (rx-test-suite (cons test-case test-case-list)) survived killed not-well-formed)]))
(define (add-survived gen-result mutant)
  (struct-copy test-suite-gen-result gen-result
               [survived (cons mutant (test-suite-gen-result-survived gen-result))]))
(define (add-killed gen-result mutant)
  (struct-copy test-suite-gen-result gen-result
               [killed (cons mutant (test-suite-gen-result-killed gen-result))]))
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
    [(test-suite-gen-result suite survived killed not-well-formed)
     (format "suite:\n~a\nsurvived:\n~a\nkilled:\n~a\nnot-well-formed:\n~a\n"
             (format-suite suite)
             (format-specs survived)
             (format-specs killed)
             (format-specs not-well-formed))]))

(define (find-distinguish-input spec1 spec2 bindings sym-trace)
  (synthesize
   #:forall (list)
   #:guarantee
   (assert (not
            (equal? (interpret-spec spec1 sym-trace bindings)
                    (interpret-spec spec2 sym-trace bindings))))))

(define (check-semantics spec-input sym-trace binding-input)
  (sat? (synthesize #:forall (symbolics sym-trace)
                    #:guarantee (assert (interpret-spec spec-input sym-trace binding-input)))))

(define (gen-test-on-mutants spec mutants bindings constructor-list start-len max-len step)
  (define (gen-test-on-mutant mutant cur-len)
    ;semantics should already be checked
    (let* ([sym-trace (get-symbolic-trace constructor-list cur-len)]
           [model (find-distinguish-input spec mutant bindings sym-trace)])
      (if (unsat? model)
          (let ([new-len (+ cur-len step)])
            (if (> new-len max-len)
                'survived
                (gen-test-on-mutant mutant new-len)))
          (let* ([trace (evaluate sym-trace (complete-solution model (symbolics sym-trace)))]
                 [output (interpret-spec spec trace bindings)])
            (rx-test-case trace output)))))
  (define (iter mutants)
    (match mutants
      [(list) (test-suite-gen-result (rx-test-suite '()) '() '() '())]
      [(cons mutant rest)
       (let ([sym-trace (get-symbolic-trace constructor-list max-len)])
         (if (check-semantics mutant sym-trace bindings)
             (let ([result (gen-test-on-mutant mutant start-len)])
               (if (eq? 'survived result)
                   (add-survived (iter rest) mutant)
                   (add-test (add-killed (iter rest) mutant) result)))
             (add-not-well-formed (iter rest) mutant)))]))
  (iter mutants))
           
      

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

     (define sym-trace (get-symbolic-trace
                        constructor-list
                        5))


     (define result (time (format-result (gen-test-on-mutants spec1-input (list spec2-input spec3-input) binding-input constructor-list 5 20 5))))

    (displayln result)
     
     )
  (test-case2)
  )
(main)