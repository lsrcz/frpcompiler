#lang racket

(require "../ir/inst.rkt")

(provide print-inst-list)
(struct reg-inst (inst reg) #:transparent)

(define (format-inst-list ir-list-input)
      (define (get-ident ident)
      (if (= ident 0)
          ""
          (string-append " " (get-ident (- ident 1)))))
  (define (format-inst-imperative ident imperative-inst)
    (match imperative-inst
      [(list 'bind name value next-inst)
       (string-append (format "~aconst ~a = ~a;\n"
                              (get-ident ident)
                              name
                              value)
                      (format-inst-imperative ident next-inst))]
      [(list 'if arg branch)
       (string-append (format "~aif (~a) {\n" (get-ident ident) arg)
                      (format-inst-imperative (+ 2 ident) branch)
                      (format "~a}\n" (get-ident ident)))]
      [(list 'if-else arg then-branch else-branch)
       (string-append (format "~aif (~a) {\n" (get-ident ident) arg)
                      (format-inst-imperative (+ 2 ident) then-branch)
                      (format "~a} else {\n" (get-ident ident))
                      (format-inst-imperative (+ 2 ident) else-branch)
                      (format "~a}\n" (get-ident ident)))]
      [(list 'empty-stream) (format "~areturn NEVER;\n" (get-ident ident))]
      [(ir-list lst _) (format-inst-list-inner ident imperative-inst)]))
  (define (format-inst-list-inner ident ir-list-input)
    (define inst-list (ir-list-lst ir-list-input))
    (define ref-table-list (ir-list-ref-table-lst ir-list-input))

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
        [(intro-const-inst intro-lst ref shape) (format "~a = ~a.intro-const(~a) :: ~a" (inst->str inst) (inst->str ref) intro-lst shape)]
        [(compute-inst to-compute name ref shape) (format "~a = ~a.compute(~a, ~a) :: ~a" (inst->str inst) (inst->str ref) name to-compute shape)]
        [(filter-inst arg ref shape) (format "~a = ~a.filter(~a) :: ~a" (inst->str inst) (inst->str ref) arg shape)]
        [(partition-inst arg ref shape) (format "~a = ~a.partition(~a) :: ~a" (inst->str inst) (inst->str ref) arg shape)]
        [(ret-inst arg ref) (format "~a = ~a.return(~a)" (inst->str inst) (inst->str ref) arg)]
        [(ret-action-inst return-val action ref) (format "~a = ~a.retact(~a => ~a)" (inst->str inst) (inst->str ref) return-val action)]
        [(merge-inst to-merge) (format "~a = merge(~a)" (inst->str inst) (map inst->str to-merge))]
        [(merge-action-inst to-merge) (format "~a = mergeact(~a)" (inst->str inst) (map inst->str to-merge))]
        [(custom-inst name ref shape) (format "~a = ~a(~a) :: ~a" (inst->str inst) name (inst->str ref) shape)]
        [(split-inst bindings imperative ref)
         (string-append
          (format "~a = ~a.split(~a => {\n" (inst->str inst) (inst->str ref) bindings)
          (format-inst-imperative (+ 2 ident) imperative)
          (format "~a})" (get-ident ident))
          )]))
    (define (iter inst-list)
      (if (null? inst-list)
          ""
          (string-append (format "~a~a\n" (get-ident ident) (format-inst (car inst-list)))
                         (iter (cdr inst-list)))))
    (string-append (format "~a// ~a\n" (get-ident ident) ref-table-list) (iter inst-list)))
  (format-inst-list-inner 0 ir-list-input))

(define (print-inst-list ir-list-input)
  (display (format-inst-list ir-list-input)))