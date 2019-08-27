#lang racket

(require "../ast/find-missing.rkt")
(require "../ast/find-retval.rkt")
(require "../ast/syntax.rkt")
(require "inst.rkt")
(require "../print/ir.rkt")
(require "../util/util.rkt")
(require "../test/test-spec.rkt")
(require "../ast/nested-ref.rkt")

(provide (all-defined-out))

(define (analyze-prev inst)
  (define (analyze-prev-if inst)
    (analyze-prev (if-branch inst)))
  (define (analyze-prev-if-else inst)
    (append (analyze-prev (if-else-then-branch inst)) (analyze-prev (if-else-else-branch inst))))
  (define (analyze-prev-return inst)
    '())
  (define (analyze-prev-bind inst)
    (define (iter p)
      (cond [(null? p) '()]
            [(list? p)
             (if (eq? (car p) 'prev)
                 (list p)
                 (append (iter (car p)) (iter (cdr p))))]
            [else '()]))
    (append (iter (bind-body inst)) (analyze-prev (bind-inst inst))))
  (define (analyze-prev-custom inst)
    (analyze-prev (custom-body inst)))
  (define (analyze-prev-split inst)
    '())
  (cond [(if? inst)
         (analyze-prev-if inst)]
        [(if-else? inst)
         (analyze-prev-if-else inst)]
        [(bind? inst)
         (analyze-prev-bind inst)]
        [(return? inst)
         (analyze-prev-return inst)]
        [(custom? inst)
         (analyze-prev-custom inst)]
        [(split? inst)
         (analyze-prev-split inst)]
        [else (error "should not happen")]))

(define (translate-body inputs output funclist should-emit-action constantlist body)
  (define (translate-imperative constantlist body)
    (match body
      [(list 'bind name body inst)
       (build-bind name body (translate-imperative (add-ref name constantlist) inst))]
      [(list 'if arg branch)
       (build-if arg (translate-imperative constantlist branch))]
      [(list 'if-else arg then-branch else-branch)
       (build-if-else arg (translate-imperative constantlist then-branch) (translate-imperative constantlist else-branch))]
      [(list 'empty-stream) (ir-list (list (empty-inst)) constantlist)]
      [(list 'new-stream body) (translate-new-stream constantlist body)]
      [_ (error "error pattern")]))
  (define (translate-new-stream constantlist body)
    (define (gen-merge inst-list)
      (let ([ret-list (filter (lambda (x) (or (split-inst? x)
                                              (if should-emit-action (ret-action-inst? x) (ret-inst? x))))
                              inst-list)])
        (if should-emit-action
            (append inst-list (list (merge-action-inst ret-list)))
            (append inst-list (list (merge-inst ret-list))))))
    (define (translate start-input constantlist inst)
      (define (wrap-in-list s)
        (if (list? s) s (list s)))
      (define (compute-intro-shape intros intro-mapping shape)
        (define (get-raw-intro intro)
          (if (list? intro)
              (get-raw-intro (cadr intro))
              intro))
        (define (concat-shape s1 s2)
          (let ([s1n (wrap-in-list s1)]
                [s2n (wrap-in-list s2)])
            (append s1n s2n)))
        (define (add-one-shape intro last-shape)
          (let ([inst (cdr (assoc intro intro-mapping))])
            (concat-shape last-shape (get-shape inst))))
        (define (iter intros last-shape)
          (if (null? intros)
              last-shape
              (iter (cdr intros) (add-one-shape (car intros) last-shape))))
        (let ([base (wrap-in-list shape)])
          (iter intros base)))
      (define (args-to-input-insts intro-mapping args)
        (map (lambda (x) (cdr (assoc x intro-mapping))) args))
      (define (possible-intro intro-mapping ref arg)
        (let* ([shape (get-shape ref)])
          (if
           (if (list? shape)
               (member arg shape)
               (eq? arg shape))
           #f
           (if (resolve-ref arg constantlist)
               (intro-const-inst (list arg) ref (if (list? shape) (append shape (list arg)) (list shape arg)))
               (intro-inst (args-to-input-insts intro-mapping (list arg))
                           ref (compute-intro-shape (list arg) intro-mapping shape))))))
      (define (translate-inst intro-mapping ref inst)
        ((cond [(return-val-found? output inst) translate-return-val-found]
               [(bind? inst) translate-bind]
               [(return? inst) translate-return]
               [(if? inst) translate-if]
               [(if-else? inst) translate-if-else]
               [(custom? inst) translate-custom]
               [(split? inst) translate-split]
               [else (error "not implemented")])
         intro-mapping ref inst))
      (define (translate-split intro-mapping ref inst)
        (define (translate-final-split bindings intro-list body)
          (let* ([shape (get-shape ref)]
                 [new-intro-inst
                  (if (null? intro-list)
                      #f
                      (intro-inst
                       (args-to-input-insts intro-mapping intro-list)
                       ref
                       (compute-intro-shape intro-list intro-mapping shape)))]
                 [split-ref (if new-intro-inst new-intro-inst ref)]
                 [new-split-inst (split-inst
                                  bindings
                                  (translate-imperative (append (map car bindings) constantlist) body)
                                  split-ref)])
            (if new-intro-inst
                (list new-intro-inst new-split-inst)
                (list new-split-inst))))
        (define (translate-restructure-split bindings processed-bindings-rev intro-list body)
          (if (null? bindings)
              (translate-final-split (reverse processed-bindings-rev) intro-list body)
              (let* ([shape (get-shape ref)]
                     [cur (car bindings)]
                     [name (split-binding-name cur)]
                     [arg (split-binding-body cur)])
                (if
                 (if (list? shape)
                     (member arg shape)
                     (eq? arg shape))
                 (translate-restructure-split (cdr bindings) (cons cur processed-bindings-rev) intro-list body)
                 (if (resolve-ref arg constantlist)
                     (translate-restructure-split (cdr bindings) processed-bindings-rev intro-list
                                        (build-bind name arg body))
                     (translate-restructure-split (cdr bindings) (cons cur processed-bindings-rev)
                                        (cons arg intro-list) body))))))
        (match inst
          [(list 'split bindings body)
           (translate-restructure-split bindings '() '() body)]))
        
      (define (translate-return-val-found intro-mapping ref inst)
        (let* ([shape (get-shape ref)]
               [missing (find-missing-var-deep (wrap-in-list shape) funclist output constantlist inst)])
          (match missing
            [(all-found)
             (list (ret-action-inst output inst ref))]
            [(not-found (list) _) (list (ret-action-inst output inst ref))]
            [(not-found intros _)
             (let* ([intros-shape (compute-intro-shape intros intro-mapping shape)]
                    [new-intro-inst
                     (intro-inst
                      (args-to-input-insts intro-mapping intros)
                      ref
                      intros-shape)]
                    [new-ret-action-inst
                     (ret-action-inst output inst new-intro-inst)])
               (list new-intro-inst new-ret-action-inst))])))
      (define (translate-bind intro-mapping ref inst)
        (let* ([shape (get-shape ref)]
               [missing (find-missing-var (wrap-in-list shape) funclist output constantlist (bind-body inst))])
          (match missing
            [(all-found)
             (let ([new-inst
                    (compute-inst
                     (bind-body inst)
                     (bind-name inst)
                     ref
                     (if (list? shape)
                         (append shape (list (bind-name inst)))
                         (cons shape (list (bind-name inst)))))])
               (cons new-inst
                     (translate-inst intro-mapping new-inst (bind-inst inst))))]
            [(not-found (list) _)
             (let ([new-inst
                    (compute-inst
                     (bind-body inst)
                     (bind-name inst)
                     ref
                     (if (list? shape)
                         (append shape (list (bind-name inst)))
                         (cons shape (list (bind-name inst)))))])
               (cons new-inst
                     (translate-inst intro-mapping new-inst (bind-inst inst))))]
            [(not-found intros _)
             (let* ([intros-shape (compute-intro-shape intros intro-mapping shape)]
                    [new-intro-inst
                     (intro-inst
                      (args-to-input-insts intro-mapping intros)
                      ref
                      intros-shape)]
                    [new-compute-inst
                     (compute-inst
                      (bind-body inst)
                      (bind-name inst)
                      new-intro-inst
                      (append intros-shape (list (bind-name inst))))])
               (cons new-intro-inst
                     (cons new-compute-inst
                           (translate-inst intro-mapping new-compute-inst (bind-inst inst)))))])))
      (define (translate-return intro-mapping ref inst)
        (let* ([arg (return-arg inst)]
               [intro-inst (possible-intro intro-mapping ref arg)]
               [next-ref (if intro-inst intro-inst ref)]
               [new-inst
                (if should-emit-action
                    (ret-action-inst output (list 'return arg) next-ref)
                    (ret-inst arg next-ref))])
          (if intro-inst
              (list intro-inst new-inst)
              (list new-inst))))
      (define (translate-if intro-mapping ref inst)
        (let* ([arg (if-arg inst)]
               [intro-inst (possible-intro intro-mapping ref arg)]
               [next-ref (if intro-inst intro-inst ref)]
               [shape (get-shape next-ref)]
               [new-inst
                (filter-inst (if-arg inst) next-ref shape)]
               [more-inst (translate-inst intro-mapping new-inst (if-branch inst))]
               [inst-list (cons new-inst more-inst)])
          (if intro-inst
              (cons intro-inst inst-list)
              inst-list)))
      (define (translate-if-else intro-mapping ref inst)
        (let* ([arg (if-else-arg inst)]
               [intro-inst (possible-intro intro-mapping ref arg)]
               [next-ref (if intro-inst intro-inst ref)]
               [shape (get-shape next-ref)]
               [new-inst
                (partition-inst (if-else-arg inst) next-ref shape)]
               [then-inst-list
                (translate-inst intro-mapping (cons new-inst 0) (if-else-then-branch inst))]
               [else-inst-list
                (translate-inst intro-mapping (cons new-inst 1) (if-else-else-branch inst))]
               [inst-list (cons new-inst (append then-inst-list else-inst-list))])
          (if intro-inst
              (cons intro-inst inst-list)
              inst-list)))
      (define (translate-custom intro-mapping ref inst)
        (let* ([name (custom-name inst)]
               [shape (get-shape ref)]
               [new-inst (custom-inst name ref shape)]
               [body-inst-list (translate-inst intro-mapping new-inst (custom-body inst))])
          (cons new-inst body-inst-list)))
      (define (unfold-prev prev)
        (if (list? prev)
            (match (unfold-prev (cadr prev))
              [(list name num) (list name (+ 1 num))])
            (list prev 0)))
      (define (collect-prev prevs)
        (define (update inputs-map name num)
          (if (null? inputs-map)
              (error "should not happen")
              (if (eq? (caar inputs-map) name)
                  (cons (list name (max num (cadar inputs-map))) (cdr inputs-map))
                  (cons (car inputs-map) (update (cdr inputs-map) name num)))))
        (define (iter inputs-map prevs)
          (if (null? prevs)
              inputs-map
              (let* ([prev (car prevs)]
                     [unfolded (unfold-prev prev)])
                (match unfolded
                  [(list name num)
                   (iter (update inputs-map name num) (cdr prevs))]))))
        (iter (map (lambda (x) (list x 0)) inputs) prevs))
      (define (prev-map-to-inputs-map inputs-prev-map)
        (define (build-shape name num)
          (cond [(= num 0) name]
                [(= num 1) (list name (list 'prev name))]
                [else (cons name (map (lambda (x) (list 'prev x)) (build-shape name (- num 1))))]))
        (define (build-input-inst inputs-prev)
          (match inputs-prev
            [(list name num) (input-inst name num (build-shape name num))]))
        (map (lambda (x) (match x [(list name num) (cons name (build-input-inst x))])) inputs-prev-map))
      (let* ([prevs (analyze-prev inst)]
             [inputs-prev-map (collect-prev prevs)]
             [inputs-map (prev-map-to-inputs-map inputs-prev-map)]
             [inst-lst (map cdr inputs-map)]
             [start-ref (cdr (assoc start-input inputs-map))])
        (append inst-lst (translate-inst inputs-map start-ref inst))))
    (define (iter body)
      (define (translate-one spec-one)
        (translate (car spec-one) constantlist (cadr spec-one)))
      (if (null? body)
          '()
          (append (translate-one (car body))
                  (iter (cdr body)))))
    (ir-list (gen-merge (iter body)) constantlist))
  (translate-new-stream constantlist body))

(define (translate-spec spec-input)
  (match spec-input
    [(spec inputs output funclist constantlist body)
     (let ([should-emit-action (return-val-found-multiple? output (map cadr body))])
       (translate-body inputs output funclist should-emit-action constantlist body))]))

(define (main)
  (define spec
    '(bind _temp3
           (f mode)
           (if _temp3
               (bind _temp2
                     (m drawing)
                     (if-else _temp2
                              (bind _temp0
                                    (l down move)
                                    (return _temp0))
                              (bind _temp1
                                    (n drawing (prev move) move)
                                    (return _temp1)))))))
  (define spec1 '((move (bind a (f (prev move)) (return a)))))
  (define spec2 '((move (bind a (f move) (return a)))))
  (define spec3 '((move (bind a (f move down) (return a)))))
  (define spec4 '((move (bind a (f move) (if a (return a))))))
  (define spec5 '((move (bind a (f move) (bind b (g down) (if-else a (return a) (return b)))))))
  (define spec6 '((move (if-else move (return drawing) (return down)))))
  (define spec7 '((move (bind a (f move) (custom c (return a))))))
  (define spec8 '((move (bind a (f b) (custom c (return b))))))
  (define spec9 '((move (if b (return move)))))
  (define spec10 '((move (bind _temp0 (f p)
                               (bind _temp1 (g q)
                                     (split ((a _temp0) (b _temp1) (c mode))
                                            (bind _temp2 (f a)
                                                  (if _temp2
                                                      (bind _temp3 (g b)
                                                            (if _temp3 (new-stream ((mode (return mode))))))))))))))
  (println (analyze-prev spec))
  (println "--1--")
  (print-inst-list (translate-body '(move) 'drawing '(f) #f '() spec1))
  (println "--2--")
  (print-inst-list (translate-body '(move) 'drawing '(f) #f '() spec2))
  (println "--3--")
  (print-inst-list (translate-body '(move down) 'drawing '(f) #f '() spec3))
  (println "--4--")
  (print-inst-list (translate-body '(move) 'drawing '(f) #f '() spec4))
  (println "--5--")
  (print-inst-list (translate-body '(move down) 'drawing '(f g) #f '() spec5))
  (println "--6--")
  (print-inst-list (translate-body '(move down) 'drawing '() #t '() spec6))
  (println "--7--")
  (print-inst-list (translate-body '(move) 'drawing '(f) #f '() spec7))
  (println "--8--")
  (print-inst-list (translate-body '(move) 'drawing '(f) #f '(b) spec8))
  (println "--9--")
  (print-inst-list (translate-body '(move) 'drawing '() #f '(b) spec9))
  (println "--10--")
  (print-inst-list (translate-body '(move mode q) 'drawing '(f g) #f '(p) spec10))
  )
  
    