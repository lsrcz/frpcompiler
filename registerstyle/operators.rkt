#lang rosette

(require "dsl.rkt")
(require "instruction.rkt")

(provide (all-defined-out))

;; We want to be able to evaluate instructions in a standardized way,
;; but the RxJS operators have different signatures.
;; For each operator, we define a struct holding the name of the operator for printing
;; a function that calls the operator given an instruction and a list of past registers
;; and a function for pretty-printing

(struct operator
  (name call print) #:transparent)

(define (call-insn op insn past-vars)
  ((operator-call op) insn past-vars))

(define (print-insn op insn varname past-vars)
    (format "  (define ~a (~a ~a))" varname (operator-name op) ((operator-print op) insn past-vars)))

;; a list of higher-order int->int functions
(define int-functions (list add1 sub1 (λ (i) 1) (λ (i) -1)))
(define int-functions-str (list "add1" "sub1" "(λ (i) 1)" "(λ (i) -1)"))
;; higher-order int->int->int functions
(define int-int-functions (list + -))
(define int-int-functions-str (list "+" "-"))
;; filter functions
(define pred-functions (list even? odd?))
(define pred-functions-str (list "even?" "odd?"))

(define rxCombineLatest-op
  (operator "rxCombineLatest"
            (λ (reg-insn past-vars) (rxCombineLatest (get-input-stream1 reg-insn past-vars)
                                                     (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))))

(define rxDistinct-op
  (operator "rxDistinct"
            (λ (reg-insn past-vars) (rxDistinct (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a" (get-input-stream1 reg-insn past-vars)))))

(define rxFilter-op
  (operator "rxFilter"
            (λ (reg-insn past-vars) (rxFilter (get-argfunc reg-insn pred-functions)
                                              (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-argfunc reg-insn pred-functions-str)
                                            (get-input-stream1 reg-insn past-vars)))))
(define rxMap-op
  (operator "rxMap"
            (λ (reg-insn past-vars) (rxMap (get-argfunc reg-insn int-functions)
                                           (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-argfunc reg-insn int-functions-str)
                                            (get-input-stream1 reg-insn past-vars)))))

(define rxMerge-op
  (operator "rxMerge"
            (λ (reg-insn past-vars) (rxMerge (get-input-stream1 reg-insn past-vars)
                                             (get-input-stream2 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-input-stream1 reg-insn past-vars)
                                            (get-input-stream2 reg-insn past-vars)))))

(define rxPairwise-op
  (operator "rxPairwise"
            (λ (reg-insn past-vars) (rxPairwise (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a" (get-input-stream1 reg-insn past-vars)))))

(define rxScan-op
  (operator "rxScan"
            (λ (reg-insn past-vars) (rxScan (get-argfunc reg-insn int-int-functions)
                                            (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-argfunc reg-insn int-int-functions)
                                            (get-input-stream1 reg-insn past-vars)))))

(define rxSkip-op
  (operator "rxSkip"
            (λ (reg-insn past-vars) (rxSkip (get-const reg-insn)
                                            (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-const reg-insn)
                                            (get-input-stream1 reg-insn past-vars)))))

(define rxTake-op
  (operator "rxTake"
            (λ (reg-insn past-vars) (rxTake (get-const reg-insn)
                                            (get-input-stream1 reg-insn past-vars)))
            (λ (reg-insn past-vars) (format "~a ~a"
                                            (get-const reg-insn)
                                            (get-input-stream1 reg-insn past-vars)))))

(define operator-list (list rxCombineLatest-op rxDistinct-op rxFilter-op rxMap-op rxMerge-op rxPairwise-op rxScan-op rxSkip-op rxTake-op))
(define operator-id-lookup (make-hash (list (cons "rxCombineLatest" 0)
                                            (cons "rxDistinct" 1)
                                            (cons "rxFilter" 2)
                                            (cons "rxMap" 3)
                                            (cons "rxMerge" 4)
                                            (cons "rxPairwise" 5)
                                            (cons "rxScan" 6)
                                            (cons "rxSkip" 7)
                                            (cons "rxTake" 8))))