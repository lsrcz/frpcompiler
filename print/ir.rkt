#lang racket

(require "../ir/inst.rkt")

(provide print-inst-list)
(struct reg-inst (inst reg) #:transparent)

(define (print-inst-list inst-list)
  (define (add-reg inst-list num)
      (if (null? inst-list)
          '()
          (cons
           (reg-inst (car inst-list) (string->symbol (format "r~a" num)))
           (add-reg (cdr inst-list) (+ 1 num)))))
  (define regged-inst-list (add-reg inst-list 0))
  (define (inst->reg inst)
    (if (pair? inst)
        (cons (inst->reg (car inst)) (cdr inst))
        (reg-inst-reg (car (filter (lambda (x) (match x [(reg-inst inst1 reg) (eq? inst inst1)])) regged-inst-list)))))
  (define (reg->str reg)
    (if (pair? reg)
        (format "~a[~a]" (car reg) (cdr reg))
        (format "~a" reg)))
  (define (inst->str inst)
    (reg->str (inst->reg inst)))
  (define (format-inst inst)
    (match inst
      [(input-inst name num shape) (format "~a = input(~a, ~a) :: ~a" (inst->str inst) name num shape)]
      [(intro-inst intro-lst ref shape) (format "~a = ~a.intro(~a) :: ~a" (inst->str inst) (inst->str ref) (map inst->str intro-lst) shape)]
      [(compute-inst to-compute name ref shape) (format "~a = ~a.compute(~a, ~a) :: ~a" (inst->str inst) (inst->str ref) name to-compute shape)]
      [(filter-inst arg ref shape) (format "~a = ~a.filter(~a) :: ~a" (inst->str inst) (inst->str ref) arg shape)]
      [(partition-inst arg ref shape) (format "~a = ~a.partition(~a) :: ~a" (inst->str inst) (inst->str ref) arg shape)]
      [(ret-inst arg ref) (format "~a = ~a.return(~a)" (inst->str inst) (inst->str ref) arg)]
      [(ret-action-inst return-val action ref) (format "~a = ~a.retact(~a => ~a)" (inst->str inst) (inst->str ref) return-val action)]
      [(merge-inst to-merge) (format "~a = merge(~a)" (inst->str inst) (map inst->str to-merge))]
      [(merge-action-inst to-merge) (format "~a = mergeact(~a)" (inst->str inst) (map inst->str to-merge))]))
  (define (iter inst-list)
    (if (null? inst-list)
        (void)
        (begin
          (displayln (format-inst (car inst-list)))
          (iter (cdr inst-list)))))
  (iter inst-list))
