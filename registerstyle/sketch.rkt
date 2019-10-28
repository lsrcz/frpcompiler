#lang rosette

(require "model.rkt")
(require "instruction.rkt")
(require "operators.rkt")

(provide (all-defined-out))

;; A sketch is an incomplete program that will be filled out by the solver
;; It contains a list of symbolic instructions and the number of inputs to the program
;; We also specify the register that holds the return value of the program after evaluation
;; We do this so that our program can be up to n instructions long, instead of exactly n instructions

(struct sketch (insns retval-idx input-count) #:transparent)

(define (make-symbolic-retval-idx)
  (define-symbolic* retval-idx integer?)
  retval-idx)

(define (make-symbolic-sketch insn-count input-count)
  (sketch (make-sym-insn-list insn-count) (make-symbolic-retval-idx) input-count))

;; Given a sketch, an index pointing to a particular instruction in that sketch,
;; and an optional model found by the solver, return an operator used by that instruction

(define (operator-lookup sk insn-idx [binding #f])
  (let* ([reg-insn (if binding
                   (evaluate (list-ref (sketch-insns sk) insn-idx) binding)
                   (list-ref (sketch-insns sk) insn-idx))])
    (get-operator reg-insn operator-list)))

;; A recursive function that turns a sketch into a function that can be called on inputs
;; Works for both concrete and symbolic sketches

(define (get-sketch-function sk)
  (letrec ([f (λ (calculated-streams i)
                (cond [(equal? (length (sketch-insns sk)) i) calculated-streams]
                      [else (let ([next-stream (call-insn (operator-lookup sk i)
                                                          (list-ref (sketch-insns sk) i)
                                                          calculated-streams)])
                              (f (append calculated-streams (list next-stream)) (add1 i)))]))])
    (λ inputs (list-ref (f inputs 0) (sketch-retval-idx sk)))))

;; pretty-print a concrete sketch

(define (string-from-sketch sk funcname)
  (let* ([input-count (sketch-input-count sk)]
         [arg-list (for/list ([i (range input-count)])
                     (format "input~a" (add1 i)))]
         [input-stmt-list (for/list ([i (range input-count)])
                            (format "  (define r~a input~a)" (add1 i) (add1 i)))]
         [insn-count (length (sketch-insns sk))]
         [varlist (for/list ([i (range (+ input-count insn-count))])
                    (format "r~a" (add1 i)))]
         [stmt-list (for/list ([i (range insn-count)])
                      (print-insn (operator-lookup sk i)
                                  (list-ref (sketch-insns sk) i)
                                  (list-ref varlist (+ input-count i))
                                  (take varlist (+ input-count i))))]
         [return-stmt (format "  ~a)" (list-ref varlist (sketch-retval-idx sk)))])
    (string-append (format "(define (~a ~a)\n" funcname (string-join arg-list))
                   (string-join input-stmt-list "\n")
                   "\n"
                   (string-join stmt-list "\n")
                   "\n"
                   return-stmt)))

;; pretty-print a symbolic sketch with model from solver

(define (print-sketch sk binding [funcname "synthesized-function"])
  (displayln (string-from-sketch (evaluate sk binding) funcname)))