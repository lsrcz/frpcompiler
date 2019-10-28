#lang rosette/safe

(require rosette/lib/match
         "../interpret/interpret-spec/spec.rkt"
         rosette/base/struct/struct)

(provide (all-defined-out))

(struct rx-test-case (trace output) #:transparent)
(struct rx-test-suite (test-case-list) #:transparent)

(define (merge-rx-test-suite suite1 suite2)
  (rx-test-suite (append (rx-test-suite-test-case-list suite1)
                         (rx-test-suite-test-case-list suite2))))

(struct success-test (test-case) #:transparent)
(struct failing-test (test-case output) #:transparent)
(struct rx-test-result (success-list failing-list) #:transparent)

(define (add-result rx-test-result-input result)
  (match rx-test-result-input
    [(rx-test-result success-list failing-list)
     (match result
       [(success-test _) (rx-test-result (cons result success-list) failing-list)]
       [(failing-test _ _) (rx-test-result success-list (cons result failing-list))])]))

(define (all-pass? result) (null? (rx-test-result-failing-list result)))

(define (run-case interpreter spec-input test-case-input)
  (match test-case-input
    [(rx-test-case trace output)
     (let ([result (interpreter spec-input trace)])
       (if (equal? result output)
           (success-test test-case-input)
           (failing-test test-case-input result)))]))

(define (run-suite interpreter spec-input test-suite-input)
  (define (iter test-case-list)
    (match test-case-list
      [(list) (rx-test-result '() '())]
      [(cons test-case-input rest)
       (add-result (iter rest) (run-case interpreter spec-input test-case-input))]))
  (iter (rx-test-suite-test-case-list test-suite-input)))

