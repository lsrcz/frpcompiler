#lang rosette/safe

(require "../ast/collect-select-func.rkt"
         "../util/util.rkt"
         "../interpret/interpret-spec/spec.rkt"
         "../test/trace.rkt"
         "test-suite.rkt"
         "../interpret/interpret-spec/environment.rkt"
         "../ast/spec.rkt"
         "mutation.rkt"
         "mut-gen.rkt"
         rosette/lib/angelic)

(provide (all-defined-out))

(struct test-suite-refine-result (suite refining-bindings) #:transparent)

(define (get-sym-func collected)
  (if (boolean-func? collected)
      (let ([argnum (boolean-func-argnum collected)])
        (define-symbolic* f (apply ~> (make-list (+ 1 argnum) boolean?)))
        f)
      (let ([argnum (int-func-argnum collected)])
        (define-symbolic* f (apply ~> (append (make-list argnum boolean?) (list integer?))))
        f)))

(define (get-sym-func-constraint f collected)
  (if (boolean-func? collected)
      (cons '() #t)
      (begin
        (define (build-symbolic-list num)
          (if (= num 0)
              '()
              (begin
                (define-symbolic* x boolean?)
                (cons x (build-symbolic-list (- num 1))))))
        (let* ([argnum (int-func-argnum collected)]
               [branchnum (int-func-branchnum collected)]
               [symboliclist (build-symbolic-list argnum)])
          (cons symboliclist (and (>= (apply f symboliclist) 0) (< (apply f symboliclist) branchnum)))))))
          

(define (find-new-suite spec suite sym-trace bindings collected)
  (define (interpreter spec trace) (interpret-spec spec trace bindings))
  (define new-rx-test-suite (rx-test-suite (list (rx-test-case sym-trace (interpreter spec sym-trace)))))
  (define sym-func (get-sym-func collected))
  (define (new-interpreter spec trace) (interpret-spec spec trace (cons (binding (collected-name collected) sym-func) bindings)))
  (define sym-func-constraint-pair (get-sym-func-constraint sym-func collected))
  (define sym-func-constraint-forall (car sym-func-constraint-pair))
  (define sym-func-constraint-body (cdr sym-func-constraint-pair))
  (define m (synthesize #:forall 'sym-func-constraint-forall
                        #:guarantee (begin
                                      (assert sym-func-constraint-body)
                                      (assert (all-pass? (run-suite new-interpreter spec suite)))
                                      (assert (not (all-pass? (run-suite new-interpreter spec new-rx-test-suite)))))))
  (if (unsat? m)
      #f
      (begin
        (define new-test-suite 
          (evaluate new-rx-test-suite (complete-solution m (symbolics new-rx-test-suite))))
        (displayln "new test suite")
        (displayln new-test-suite)
        new-test-suite)))
                        

(define (refine-binding-by-select-func spec bindings suite constructor-list start-len max-len step)
  (define (interpreter spec trace) (interpret-spec spec trace bindings))
  (define collected-list (collect-select-func spec))
  (define (iter collected-list suite)
    (define (iter-inner collected len suite)
      (if (> len max-len)
          suite
        (let* ([sym-trace (get-symbolic-trace constructor-list len)]
                 [found (find-new-suite spec suite sym-trace bindings collected)])
            (if found
                (iter-inner collected len (merge-rx-test-suite suite found))
                (iter-inner collected (+ step len) suite)))))
    (if (null? collected-list)
        suite
        (iter (cdr collected-list) (iter-inner (car collected-list) start-len suite))))
  (iter collected-list suite))


(module+ test
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

    (define generated (time (gen-test-on-mutants sprinkler-spec mutants interpreter constructor-list 1 5 1)))

    ;(displayln (format-result generated))

    (define suite (test-suite-gen-result-suite generated))
    ;(define suite (rx-test-suite '()))
    (displayln (length (rx-test-suite-test-case-list suite)))

    (define result (refine-binding-by-select-func sprinkler-spec binding-input suite constructor-list 1 5 1))
    (displayln result)
    (displayln (verify (all-pass? (run-suite interpreter sprinkler-spec result))))
    (displayln (length (rx-test-suite-test-case-list result)))
    (void)
    )
    (test-case5)
  )
