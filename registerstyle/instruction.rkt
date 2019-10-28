#lang rosette

(provide (all-defined-out))

;; Our program sketches are written in a register style
;; Each register holds the output of a single instruction
;; An instruction consists of an operator from our DSL and its arguments
;; A symbolic instruction represents a choice between all possible operators
;; and all possible arguments
;; We represent a symbolic operator as a concrete list with a symbolic index indicating
;; the solvers choice.
;; Arguments are represented the same way

;; For the single-instruction program
;; stream3.map(add1)
;; given the possible operators [map, merge, scan]
;; possible functions [add1, sub1]
;; and possible argument streams [stream1, stream2, stream3]
;; the instruction would be {0 0 2}

(struct insn
  (op-idx input-idx1 input-idx2 argfunc-idx1 const-int) #:transparent)

;; helpers

(define (get-operator insn operators)
  (list-ref operators (insn-op-idx insn)))

(define (get-input-stream1 insn streams)
  (list-ref streams (insn-input-idx1 insn)))

(define (get-input-stream2 insn streams)
  (list-ref streams (insn-input-idx2 insn)))

(define (get-argfunc insn argfuncs)
  (list-ref argfuncs (insn-argfunc-idx1 insn)))

(define (get-const insn)
  (insn-const-int insn))

;; create symbolic instructions

(define (make-sym-insn)
  (define-symbolic* op-idx integer?)
  (define-symbolic* input-idx1 integer?)
  (define-symbolic* input-idx2 integer?)
  (define-symbolic* argfunc-idx1 integer?)
  (define-symbolic* const-int integer?)
  (insn op-idx input-idx1 input-idx2 argfunc-idx1 const-int))

(define (make-sym-insn-list count)
  (for/list ([i (range count)])
    (make-sym-insn)))