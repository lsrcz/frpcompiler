#lang racket

(require "../rxir/inst.rkt")
(require "../ast/syntax.rkt")
(provide print-rx-program)

(struct reg-inst (inst reg) #:transparent)

(define (format-rx-program inputs rx-inst-list)
  (define (add-reg inst-list num)
    (if (null? inst-list)
        '()
        (cons
         (reg-inst (car inst-list) (string->symbol (format "r~a" num)))
         (add-reg (cdr inst-list) (+ 1 num)))))
  (define regged-inst-list (add-reg rx-inst-list 0))
  (define (inst->reg inst)
    (if (pair? inst)
        (cons (inst->reg (car inst)) (cdr inst))
        (reg-inst-reg (car
                       (filter
                        (lambda (x)
                          (match x
                            [(reg-inst inst1 reg) (eq? inst inst1)]))
                        regged-inst-list)))))
  (define (reg->str reg)
    (if (pair? reg)
        (format "~a[~a]" (car reg) (cdr reg))
        (format "~a" reg)))
  (define (ref->str ref)
    (reg->str (inst->reg (rx-stream-ref-stream ref))))
  (define (pair-ref->str ref)
    (format "~a[~a]" (reg->str (inst->reg (rx-stream-pair-ref-stream ref))) (rx-stream-pair-ref-num ref)))
  (define (format-comma-sep to-str)
    (define (iter lst)
      (if (null? lst)
          ""
          (if (null? (cdr lst))
              (to-str (car lst))
              (string-append (to-str (car lst)) ", " (iter (cdr lst))))))
    iter)
  (define format-symbol-list (format-comma-sep symbol->string))
  (define format-stream-list (format-comma-sep ref->str))
  (define (format-prev p)
    (match p
      [(list 'prev n) (string-append "prev_" (format-prev n))]
      [_ (symbol->string p)]))
  (define (format-symbol symbol)
    (if (list? symbol)
        (if (eq? (car symbol) 'prev)
            (format-prev symbol)
            (string-append (format-symbol (car symbol)) "(" (format-shape-inner (cdr symbol)) ")"))
        (symbol->string symbol)))
  (define (format-shape-inner lst)
    (if (list? lst)
        ((format-comma-sep format-symbol) lst)
        (symbol->string lst)))
  (define (format-shape-with-bracket lst)
    (if (list? lst)
        (string-append "[" (format-shape-inner lst) "]")
        (symbol->string lst)))
  (define (format-nested-shape lst)
    ((format-comma-sep
      (lambda (x)
        (if (list? x)
            (string-append (format-shape-with-bracket x))
            (symbol->string x)))) lst))
  (define (format-merge m)
    (match m
      [(rx-merge lst) (string-append "merge(" (format-stream-list lst) ")")]))
  (define (format-merge-action m)
    (match m
      [(rx-merge-action lst) (format "merge(~a).pipe(\n    scan((acc, cur) => cur(acc), undefined),\n    filter(Boolean),\n  )" (format-stream-list lst))]))
  (define (format-ref r)
    (ref->str r))
  (define (format-pair-ref r)
    (pair-ref->str r))
  (define (format-stream s)
    (cond [(rx-merge? s) (format-merge s)]
          [(rx-merge-action? s) (format-merge-action s)]
          [(rx-pipe? s) (format-pipe s)]
          [(rx-name-ref? s) (format "~a" (rx-name-ref-name s))]
          [(rx-stream-ref? s) (format-ref s)]
          [(rx-stream-pair-ref? s) (format-pair-ref s)]
          [else (error "not implemented")]))
  (define (format-pipe pipe)
    (match pipe
      [(rx-pipe stream ops)
       (if (null? ops)
           (format-stream stream)
           (string-append (format-stream stream) ".pipe(\n" (format-operators ops) "  )"))]))
  (define (format-operator op)
    (define (format-rx-start-with-undefined)
      "startWith(undefined)")
    (define (format-rx-buffer-count buffer-size start-buffer-every)
      (format "bufferCount(~a, ~a)" buffer-size start-buffer-every))
    (define (format-rx-map-shape from-shape to-shape)
      (format "map((~a) => ~a)" (format-shape-with-bracket from-shape) (format-shape-with-bracket to-shape)))
    (define (format-rx-with-latest-from streams)
      (format "withLatestFrom(~a)" (format-stream-list streams)))
    (define (format-rx-map-flat from-nested-shape to-shape)
      (format "map(([~a]) => ~a)" (format-nested-shape from-nested-shape) (format-shape-with-bracket to-shape)))
    (define (format-rx-filter from-shape arg)
      (format "filter((~a) => ~a)" (format-shape-with-bracket from-shape) arg))
    (define (format-rx-partition from-shape arg)
      (format "partition((~a) => ~a)" (format-shape-with-bracket from-shape) arg))
    (define (format-rx-ret-action from-shape return-val action)
      (define (format-action action)
        (define (format-action-inner action ident)
          (define (get-ident ident)
            (if (= ident 0)
                ""
                (string-append " " (get-ident (- ident 1)))))
          (define (format-action-bind action ident)
            (string-append (get-ident ident)
                           (format "const ~a = ~a;\n" (bind-name action) (format-symbol (bind-body action)))
                           (format-action-inner (bind-inst action) ident)))
          (define (format-action-if action ident)
            (string-append (get-ident ident)
                           (format "if (~a) {\n" (if-arg action))
                           (format-action-inner (if-branch action) (+ ident 2))
                           (get-ident ident)
                           "}\n"))
          (define (format-action-if-else action ident)
            (string-append (get-ident ident)
                           (format "if (~a) {\n" (if-else-arg action))
                           (format-action-inner (if-else-then-branch action) (+ ident 2))
                           (get-ident ident)
                           "} else {\n"
                           (format-action-inner (if-else-else-branch action) (+ ident 2))
                           (get-ident ident)
                           "}\n"))
          (define (format-action-return action ident)
            (string-append (get-ident ident)
                           (format "return ~a;\n" (return-arg action))))
          ((cond [(bind? action) format-action-bind]
                 [(if? action) format-action-if]
                 [(if-else? action) format-action-if-else]
                 [(return? action) format-action-return])
           action ident))
        (format-action-inner action 6))
      (format "map((~a) => ~a => {\n~a    })" (format-shape-with-bracket from-shape) return-val (format-action action)))
    (match op
      [(rx-start-with-undefined) (format-rx-start-with-undefined)]
      [(rx-buffer-count buffer-size start-buffer-every) (format-rx-buffer-count buffer-size start-buffer-every)]
      [(rx-map-shape from-shape to-shape) (format-rx-map-shape from-shape to-shape)]
      [(rx-with-latest-from streams) (format-rx-with-latest-from streams)]
      [(rx-map-flat from-nested-shape to-shape) (format-rx-map-flat from-nested-shape to-shape)]
      [(rx-filter from-shape arg) (format-rx-filter from-shape arg)]
      [(rx-partition from-shape arg) (format-rx-partition from-shape arg)]
      [(rx-ret-action from-shape return-val action) (format-rx-ret-action from-shape return-val action)]
      [else (error "not implemented")]))
  (define (format-operators ops)
    (if (null? ops)
        ""
        (string-append "    " (format-operator (car ops)) ",\n" (format-operators (cdr ops)))))
  (define (format-inst inst)
    (format "const ~a = ~a" (format-stream (rx-stream-ref inst)) (format-stream inst)))
  (define (format-streams lst)
    (if (null? lst)
        ""
        (string-append "  " (format-inst (car lst)) ";\n" (format-streams (cdr lst)))))
  (string-append "function " "compiled(" (format-symbol-list inputs) ") {\n" (format-streams rx-inst-list) (format "  return ~a;\n" (format-stream (rx-stream-ref (last rx-inst-list)))) "}\n"))

(define (print-rx-program inputs rx-inst-list)
  (display (format-rx-program inputs rx-inst-list)))

(define (main)
  (let* ([r1 (rx-name-ref 'a)]
         [r2 (rx-pipe (rx-name-ref 'b)
                      (list
                       (rx-start-with-undefined)
                       (rx-buffer-count 2 1)))]
         [r3 (rx-pipe (rx-name-ref 'c)
                      (list
                       (rx-start-with-undefined)
                       (rx-buffer-count 2 1)))]
         [r4 (rx-pipe (rx-stream-ref r2)
                      (list (rx-map-shape '(b (prev b)) '(b (prev b) (f b (prev b))))))]
         [r5 (rx-pipe (rx-stream-ref r1)
                      (list (rx-with-latest-from (list (rx-stream-ref r2) (rx-stream-ref r3)))
                            (rx-map-flat '(a (b (prev b)) (c (prev c))) '(a b (prev b) c (prev c)))))]
         [r6 (rx-pipe (rx-stream-ref r3)
                      (list (rx-filter '(b prev_b _temp0) '_temp0)))]
         [r7 (rx-pipe (rx-stream-ref r3)
                      (list (rx-partition '(b prev_b _temp0) '_temp0)))]
         [r8 (rx-pipe (rx-stream-pair-ref r7 0)
                      (list (rx-map-shape '(b prev_b _temp0) '_temp0)))]
         [r9 (rx-merge (list (rx-stream-ref r8)))]
         [r10 (rx-merge-action (list (rx-stream-ref r8)))]
         [r11 (rx-pipe (rx-stream-ref r3)
                       (list (rx-ret-action '(c (prev c)) 'draw '(bind m (prev c) (if m (if-else m (return m) (return draw)))))))])
    (print-rx-program '(a b c)
                      (list r1
                            r2
                            r3
                            r4
                            r5
                            r6
                            r7
                            r8
                            r9
                            r10
                            r11
                            ))))
       
  