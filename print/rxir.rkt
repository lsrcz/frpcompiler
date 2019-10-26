#lang racket

(require "../rxir/inst.rkt")
(require "../ast/syntax.rkt")
(provide print-rx-program)

(struct reg-inst (inst reg) #:transparent)
(define (format-rx-program rxir-list-input)
  (define rxir-input-insts (rxir-list-input-insts rxir-list-input))
  (define rxir-input-reg-list
    (map (match-lambda
           [(rxir-input-inst name num inst)
            (reg-inst inst
                      (if (= num 0)
                          name
                          (string->symbol(format "~a~a" name num))))])
         rxir-input-insts))
  (define inputs (map (match-lambda [(rxir-input-inst name _ _) name]) rxir-input-insts))
  
  (define (format-comma-sep to-str)
    (define (iter lst)
      (if (null? lst)
          ""
          (if (null? (cdr lst))
              (to-str (car lst))
              (string-append (to-str (car lst)) ", " (iter (cdr lst))))))
    iter)
  (define format-symbol-list (format-comma-sep symbol->string))
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
  (define (get-ident ident)
    (if (= ident 0)
        ""
        (string-append " " (get-ident (- ident 1)))))
  (define (format-rx-program-inner ident rx-inst-list [gen-return #f])
    (define (add-reg inst-list num)
      (define (iter inst-list num)
        (if (null? inst-list)
            '()
            (cons
             (reg-inst (car inst-list) (string->symbol (format "r~a" num)))
             (iter (cdr inst-list) (+ 1 num)))))
      (append
       rxir-input-reg-list
       (iter inst-list num)))
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
    (define format-stream-list (format-comma-sep ref->str))
    
    (define (format-merge m)
      (match m
        [(rx-merge lst) (string-append "merge(" (format-stream-list lst) ")")]))
    (define (format-merge-action ident m)
      (match m
        [(rx-merge-action lst) (format "merge(~a).pipe(\n~ascan((acc, cur) => cur(acc), undefined),\n~afilter(Boolean),\n~a)"
                                       (format-stream-list lst)
                                       (get-ident (+ ident 2))
                                       (get-ident (+ ident 2))
                                       (get-ident ident))]))
    (define (format-merge-action-start ident m)
      (match m
        [(rx-merge-action-start lst start-val) (format "merge(~a).pipe(\n~ascan((acc, cur) => cur(acc), ~a),\n~afilter(Boolean),\n~a)"
                                                       (format-stream-list lst)
                                                       (get-ident (+ ident 2))
                                                       (format-symbol start-val)
                                                       (get-ident (+ ident 2))
                                                       (get-ident ident))]))
    (define (format-action ident a)
      (match a
        [(rx-action action) (format "~a.pipe(\n~ascan((acc, cur) => cur(acc), undefined),\n~afilter(Boolean),\n~a)"
                                    (format-stream ident action)
                                    (get-ident (+ ident 2))
                                    (get-ident (+ ident 2))
                                    (get-ident ident))]))
    (define (format-of o)
      (match o
        [(rx-of val) (format "of(~a)" (format-symbol val))]))
    (define (format-ref r)
      (ref->str r))
    (define (format-pair-ref r)
      (pair-ref->str r))
    (define (format-stream ident s)
      (cond [(rx-merge? s) (format-merge s)]
            [(rx-merge-action? s) (format-merge-action ident s)]
            [(rx-merge-action-start? s) (format-merge-action-start ident s)]
            [(rx-pipe? s) (format-pipe ident s)]
            [(rx-name-ref? s) (format "~a" (rx-name-ref-name s))]
            [(rx-stream-ref? s) (format-ref s)]
            [(rx-stream-pair-ref? s) (format-pair-ref s)]
            [(rx-custom? s) (format-custom ident s)]
            [(rx-empty? s) "NEVER"]
            [(rx-action? s) (format-action ident s)]
            [(rx-of? s) (format-of s)]
            [else (error "not implemented")]))
    (define (format-pipe ident pipe)
      (match pipe
        [(rx-pipe stream ops)
         (if (null? ops)
             (format-stream ident stream)
             (string-append (format-stream ident stream) ".pipe(\n" (format-operators (+ 2 ident) ops) (get-ident ident) ")"))]))
    (define (format-custom ident custom)
      (match custom
        ([rx-custom stream name] (format "~a(~a)" name (format-stream ident stream)))))
    (define (format-operator ident op)
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
      (define (format-rx-ret-action from-shape return-val ident action)
        (format "map((~a) => (~a) => {\n~a~a})" (format-shape-with-bracket from-shape) return-val (format-imperative action (+ 2 ident)) (get-ident ident)))
      (define (format-rx-switch-map from-shape body)
        (format "switchMap((~a) => {\n~a~a})" (format-shape-with-bracket from-shape) (format-imperative body (+ 2 ident)) (get-ident ident)))
      (define (format-rx-to-action return-val)
        (format "map((ret) => (~a) => ret)" return-val))
      (define (format-rx-scan-undefined from-shape return-val ident action)
        (format "scan((~a, ~a) => {\n~a~a}, undefined),\n~afilter(Boolean)"
                return-val
                (format-shape-with-bracket from-shape)
                (format-imperative action (+ 2 ident))
                (get-ident ident)
                (get-ident ident)))
      (define (format-rx-scan from-shape return-val start-val ident action)
        (format "scan((~a, ~a) => {\n~a~a}, ~a),\n~afilter(Boolean)"
                return-val
                (format-shape-with-bracket from-shape)
                (format-imperative action (+ 2 ident))
                (get-ident ident)
                (format-symbol start-val)
                (get-ident ident)))
      (define (format-rx-start-with val)
        (format "startWith(~a)" (format-symbol val)))
      (define (format-imperative inst ident)
        (define ident-str (get-ident ident))
        (define identp2 (+ ident 2))
        (define (format-imperative-bind inst)
          (match inst
            [(list 'bind name body (list 'return name)) (format "~areturn ~a;\n" ident-str (format-symbol body))]
            [_
             (format "~aconst ~a = ~a;\n~a"
                     ident-str
                     (bind-name inst)
                     (format-symbol (bind-body inst))
                     (format-imperative (bind-inst inst) ident))]))
        (define (format-imperative-if inst)
          (format "~aif (~a) {\n~a~a}\n"
                  ident-str
                  (if-arg inst)
                  (format-imperative (if-branch inst) identp2)
                  ident-str))
        (define (format-imperative-if-else inst)
          (format "~aif (~a) {\n~a~a} else {\n~a~a}\n"
                  ident-str
                  (if-else-arg inst)
                  (format-imperative (if-else-then-branch inst) identp2)
                  ident-str
                  (format-imperative (if-else-else-branch inst) identp2)
                  ident-str))
        (define (format-imperative-return inst)
          (format "~areturn ~a;\n" ident-str (return-arg inst)))
        (define (format-imperative-empty inst)
          (format "~areturn NEVER;\n" ident-str))
        ((cond [(rxir-list? inst) (lambda (x) (format-rx-program-inner ident (rxir-list-lst x) #t))]
               [(bind? inst) format-imperative-bind]
               [(if? inst) format-imperative-if]
               [(if-else? inst) format-imperative-if-else]
               [(return? inst) format-imperative-return])
         inst))
       (match op
         [(rx-start-with-undefined) (format-rx-start-with-undefined)]
         [(rx-buffer-count buffer-size start-buffer-every) (format-rx-buffer-count buffer-size start-buffer-every)]
         [(rx-map-shape from-shape to-shape) (format-rx-map-shape from-shape to-shape)]
         [(rx-with-latest-from streams) (format-rx-with-latest-from streams)]
         [(rx-map-flat from-nested-shape to-shape) (format-rx-map-flat from-nested-shape to-shape)]
         [(rx-filter from-shape arg) (format-rx-filter from-shape arg)]
         [(rx-partition from-shape arg) (format-rx-partition from-shape arg)]
         [(rx-ret-action from-shape return-val action) (format-rx-ret-action from-shape return-val ident action)]
         [(rx-switch-map from-shape body) (format-rx-switch-map from-shape body)]
         [(rx-to-action return-val) (format-rx-to-action return-val)]
         [(rx-scan-undefined from-shape return-val action) (format-rx-scan-undefined from-shape return-val ident action)]
         [(rx-scan from-shape return-val start-val action) (format-rx-scan from-shape return-val start-val ident action)]
         [(rx-start-with val) (format-rx-start-with val)]
         [else (error "not implemented")]))
    (define (format-operators ident ops)
      (if (null? ops)
          ""
          (string-append (get-ident ident) (format-operator ident (car ops)) ",\n" (format-operators ident (cdr ops)))))
    (define (format-inst ident inst)
      (format "const ~a = ~a" (format-stream ident (rx-stream-ref inst)) (format-stream ident inst)))
    (define (format-return ident inst)
      (format "return ~a" (format-stream ident inst)))
    (define (format-streams ident lst)
      (cond [(null? lst) ""]
            [(and gen-return (= (length lst) 1)) (string-append (get-ident ident) (format-return ident (car lst)) ";\n")]
            [else (string-append (get-ident ident) (format-inst ident (car lst)) ";\n" (format-streams ident (cdr lst)))]))
    (string-append (format-streams ident rx-inst-list)
                   #;(if gen-return
                       (format "~areturn ~a;\n" (get-ident ident) (format-stream ident (rx-stream-ref (last rx-inst-list))))
                       "")))
  (string-append "function " "compiled(" (format-symbol-list inputs) ") {\n"
                 (format-rx-program-inner 2 (map rxir-input-inst-inst (filter (match-lambda [(rxir-input-inst _ num _) (not (= num 0))]) rxir-input-insts)))
                                                    
                 (format-rx-program-inner 2 (rxir-list-lst rxir-list-input) #t) "}\n"))

(define (print-rx-program rx-inst-list)
  (display (format-rx-program rx-inst-list)))
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
    (print-rx-program (rxir-list
                       (list
                        (rxir-input-inst 'a 0 r1)
                        (rxir-input-inst 'b 1 r2)
                        (rxir-input-inst 'c 1 r3))
                       (list r4
                             r5
                             r6
                             r7
                             r8
                             r9
                             r10
                             r11
                             )))))
       
  