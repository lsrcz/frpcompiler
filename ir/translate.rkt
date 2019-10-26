#lang racket

(require "../ast/find-missing.rkt")
(require "../ast/find-retval.rkt")
(require "../ast/syntax.rkt")
(require "inst.rkt")
(require "../print/ir.rkt")
(require "../util/util.rkt")
(require "../test/test-spec.rkt")
(require "../ast/nested-ref.rkt")
(require "../ast/spec.rkt")

(provide (all-defined-out))





(define (translate-body inputs-map output funclist constantlist body)
  (define (translate-imperative constantlist body)
    (match body
      [(list 'bind name body inst)
       (build-bind name body (translate-imperative (add-ref name constantlist) inst))]
      [(list 'if arg branch)
       (build-if arg (translate-imperative constantlist branch))]
      [(list 'if-else arg then-branch else-branch)
       (build-if-else arg (translate-imperative constantlist then-branch) (translate-imperative constantlist else-branch))]
      [(list 'empty-stream) (ir-list inputs-map (list (empty-inst)) constantlist)]
      [(list 'new-stream (list)) (ir-list inputs-map (list (empty-inst)) constantlist)]
      [(list 'new-stream body)
       (translate-new-stream constantlist body)]
      [(list 'new-stream-initial (list) start-val)
       (ir-list inputs-map (list (of-inst start-val)) constantlist)]
      [(list 'new-stream-initial body start-val)
       (translate-new-stream constantlist body start-val #t)]
      [(list 'new-stream-seed (list) _) (ir-list inputs-map (list (empty-inst)) constantlist)]
      [(list 'new-stream-seed body start-val)
       (translate-new-stream constantlist body start-val #f)]
      [_ (error "error pattern")]))
  (define (translate-new-stream constantlist body [start-val #f] [should-start-with #f])
    (define should-emit-action (return-val-found-multiple-shadow? output (map cadr body)))
    (define (gen-final-ret inst-list)
      (define (purge-non-ret inst-list)
        (define (iter inst-list-rev)
          (let ([x (car inst-list-rev)])
            (if (not (or (split-inst? x)
                         (ret-action-inst? x)
                         (ret-inst? x)))
                (iter (cdr inst-list-rev))
                inst-list-rev)))
        (reverse (iter (reverse inst-list))))
      (let ([ret-list (filter (lambda (x) (or (split-inst? x) (split-action-inst? x) (ret-inst? x) (ret-action-inst? x)))
                              inst-list)])
        (if (= (length ret-list) 1)
            (let ([new-list (purge-non-ret inst-list)])
              (if should-emit-action
                  (let* ([reversed (reverse new-list)]
                         [last (car reversed)]
                         [rest-rev (cdr reversed)])
                    (match last
                      [(ret-action-inst return-val action ref)
                       (if start-val
                           (let* ([new-scan-start-inst (scan-start-inst return-val action start-val ref)]
                                  [new-start-with-inst (start-with-inst start-val new-scan-start-inst)])
                             (if should-start-with
                                 (reverse (append (list new-start-with-inst new-scan-start-inst) rest-rev))
                                 (reverse (cons new-scan-start-inst rest-rev))))
                           (reverse (cons (scan-inst return-val action ref) rest-rev)))]
                      [_ (error "should not happen")]))
                  (if (and start-val should-start-with)
                      (append new-list (list (start-with-inst start-val (last new-list))))
                      new-list)))
            (if should-emit-action
                (if start-val
                    (let* ([new-merge-action-start-inst (merge-action-start-inst ret-list start-val)]
                           [new-start-with-inst (start-with-inst start-val new-merge-action-start-inst)])
                      (if should-start-with
                          (append inst-list (list new-merge-action-start-inst new-start-with-inst))
                          (append inst-list (list new-merge-action-start-inst))))
                    (append inst-list (list (merge-action-inst ret-list))))
                (if (and start-val should-start-with)
                    (let* ([new-merge-inst (merge-inst ret-list)]
                           [new-start-with-inst (start-with-inst start-val new-merge-inst)])
                      (append inst-list (list new-merge-inst new-start-with-inst)))
                    (append inst-list (list (merge-inst ret-list))))))))

    (define (translate start-input constantlist inst)
      (define (search-inputs-map input)
        (define (iter inputs-map)
          (if (null? inputs-map)
              (error "this should not happen")
              (if (eq? (input-inst-name (car inputs-map)) input)
                  (car inputs-map)
                  (iter (cdr inputs-map)))))
        (iter inputs-map))
      (define (args-to-input-insts args)
        (map search-inputs-map args))
      (define (wrap-in-list s)
        (if (list? s) s (list s)))
      (define (compute-intro-shape intros shape)
        (define (concat-shape s1 s2)
          (let ([s1n (wrap-in-list s1)]
                [s2n (wrap-in-list s2)])
            (append s1n s2n)))
        (define (add-one-shape intro last-shape)
          (let ([inst (search-inputs-map intro)])
            (concat-shape last-shape (get-shape inst))))
        (define (iter intros last-shape)
          (if (null? intros)
              last-shape
              (iter (cdr intros) (add-one-shape (car intros) last-shape))))
        (let ([base (wrap-in-list shape)])
          (iter intros base)))

      (define (possible-intro ref arg)
        (let* ([shape (get-shape ref)])
          (if
           (if (list? shape)
               (member arg shape)
               (eq? arg shape))
           #f
           (if (resolve-ref arg constantlist)
               (intro-const-inst (list arg) ref (if (list? shape) (append shape (list arg)) (list shape arg)))
               (intro-inst (args-to-input-insts (list arg))
                           ref (compute-intro-shape (list arg) shape))))))
      (define (translate-inst ref inst)
        ((cond [(return-val-found? output inst) translate-return-val-found]
               [(bind? inst) translate-bind]
               [(return? inst) translate-return]
               [(if? inst) translate-if]
               [(if-else? inst) translate-if-else]
               [(custom? inst) translate-custom]
               [(split? inst) translate-split]
               [else (error "not implemented")])
         ref inst))
      (define (translate-split ref inst)
        (define (translate-final-split bindings intro-list body)
          (let* ([shape (get-shape ref)]
                 [new-intro-inst
                  (if (null? intro-list)
                      #f
                      (intro-inst
                       (args-to-input-insts intro-list)
                       ref
                       (compute-intro-shape intro-list shape)))]
                 [split-ref (if new-intro-inst new-intro-inst ref)]
                 [new-split-inst ((if should-emit-action (lambda (x y z) (split-action-inst output x y z)) split-inst)
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
      (define (translate-return-val-found ref inst)
        (let* ([shape (get-shape ref)]
               [missing (find-missing-var-deep (wrap-in-list shape) funclist output constantlist inst)])
          (match missing
            [(all-found)
             (list (ret-action-inst output inst ref))]
            [(not-found (list) _) (list (ret-action-inst output inst ref))]
            [(not-found intros _)
             (let* ([intros-shape (compute-intro-shape intros shape)]
                    [new-intro-inst
                     (intro-inst
                      (args-to-input-insts intros)
                      ref
                      intros-shape)]
                    [new-ret-action-inst
                     (ret-action-inst output inst new-intro-inst)])
               (list new-intro-inst new-ret-action-inst))])))
      (define (translate-bind ref inst)
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
                     (translate-inst new-inst (bind-inst inst))))]
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
                     (translate-inst new-inst (bind-inst inst))))]
            [(not-found intros _)
             (let* ([intros-shape (compute-intro-shape intros shape)]
                    [new-intro-inst
                     (intro-inst
                      (args-to-input-insts intros)
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
                           (translate-inst new-compute-inst (bind-inst inst)))))])))
      (define (translate-return ref inst)
        (let* ([arg (return-arg inst)]
               [intro-inst (possible-intro ref arg)]
               [next-ref (if intro-inst intro-inst ref)]
               [new-inst
                (if should-emit-action
                    (ret-action-inst output (list 'return arg) next-ref)
                    (ret-inst arg next-ref))])
          (if intro-inst
              (list intro-inst new-inst)
              (list new-inst))))
      (define (translate-if ref inst)
        (let* ([arg (if-arg inst)]
               [intro-inst (possible-intro ref arg)]
               [next-ref (if intro-inst intro-inst ref)]
               [shape (get-shape next-ref)]
               [new-inst
                (filter-inst (if-arg inst) next-ref shape)]
               [more-inst (translate-inst new-inst (if-branch inst))]
               [inst-list (cons new-inst more-inst)])
          (if intro-inst
              (cons intro-inst inst-list)
              inst-list)))
      (define (translate-if-else ref inst)
        (let* ([arg (if-else-arg inst)]
               [intro-inst (possible-intro ref arg)]
               [next-ref (if intro-inst intro-inst ref)]
               [shape (get-shape next-ref)]
               [new-inst
                (partition-inst (if-else-arg inst) next-ref shape)]
               [then-inst-list
                (translate-inst (cons new-inst 0) (if-else-then-branch inst))]
               [else-inst-list
                (translate-inst (cons new-inst 1) (if-else-else-branch inst))]
               [inst-list (cons new-inst (append then-inst-list else-inst-list))])
          (if intro-inst
              (cons intro-inst inst-list)
              inst-list)))
      (define (translate-custom ref inst)
        (let* ([name (custom-name inst)]
               [shape (get-shape ref)]
               [new-inst (custom-inst name ref shape)]
               [body-inst-list (translate-inst new-inst (custom-body inst))])
          (cons new-inst body-inst-list)))
      (let* ([start-ref (search-inputs-map start-input)])
        (translate-inst start-ref inst)))
    (define (iter body)
      (define (translate-one spec-one)
        (translate (car spec-one) constantlist (cadr spec-one)))
      (if (null? body)
          '()
          (append (translate-one (car body))
                  (iter (cdr body)))))
    (ir-list inputs-map (gen-final-ret (iter body)) constantlist))
  (translate-new-stream constantlist body))

(define (translate-spec spec-input)
  (define (get-inputs-map spec-input)
    (define (analyze-prev-new-stream new-stream)
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
          (analyze-prev-imperative (split-body inst)))
        (define (analyze-prev-imperative inst)
          (match inst
            [(list 'bind _ _ next) (analyze-prev-imperative next)]
            [(list 'if _ branch) (analyze-prev-imperative branch)]
            [(list 'if-else _ then-branch else-branch)
             (append (analyze-prev-imperative then-branch) (analyze-prev-imperative else-branch))]
            [(list 'empty-stream) '()]
            [(list 'new-stream new-stream) (analyze-prev-new-stream new-stream)]
            [(list 'new-stream-initial new-stream _) (analyze-prev-new-stream new-stream)]
            [(list 'new-stream-seed new-stream _) (analyze-prev-new-stream new-stream)]))
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
      (if (null? new-stream)
          '()
          (append (analyze-prev (cadr (car new-stream))) (analyze-prev-new-stream (cdr new-stream)))))

    (define (collect-prev inputs prevs)
      (define (unfold-prev prev)
        (if (list? prev)
            (match (unfold-prev (cadr prev))
              [(list name num) (list name (+ 1 num))])
            (list prev 0)))
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
      (map (lambda (x) (build-input-inst x)) inputs-prev-map))
  
    (match spec-input
      [(spec inputs _ _ _ body)
       (prev-map-to-inputs-map (collect-prev inputs (analyze-prev-new-stream body)))]))
  (match spec-input
    [(spec inputs output funclist constantlist body)
     (let ([input-map (get-inputs-map spec-input)])
       (translate-body input-map output funclist constantlist body))]))

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
  (define (name-num-to-input-list name-list num-list)
    (define (build-prev name num)
      (if (= num 0)
          name
          (list 'prev (build-prev name (- num 1)))))
    (define (build-shape name num)
      (define (iter num)
        (if (= num 0)
            (list name)
            (cons (build-prev name num) (iter (- num 1)))))
      (if (= num 0)
          name
          (iter num)))
    (if (null? name-list)
        '()
        (let ([name (car name-list)]
              [num (car num-list)])
          (append (list (input-inst name num (build-shape name num)))
                  (name-num-to-input-list (cdr name-list) (cdr num-list))))))
  (println "--1--")
  (print-inst-list (translate-body (name-num-to-input-list '(move) (list 1))
                                   'drawing '(f) '() spec1))
  (println "--2--")
  (print-inst-list (translate-body (name-num-to-input-list '(move) (list 0))
                                   'drawing '(f) '() spec2))
  (println "--3--")
  (print-inst-list (translate-body (name-num-to-input-list '(move down) (list 0 0))
                                   'drawing '(f) '() spec3))
  (println "--4--")
  (print-inst-list (translate-body (name-num-to-input-list '(move) (list 0))
                                   'drawing '(f) '() spec4))
  (println "--5--")
  (print-inst-list (translate-body (name-num-to-input-list '(move down) (list 0 0))
                                   'drawing '(f g) '() spec5))
  (println "--6--")
  (print-inst-list (translate-body (name-num-to-input-list '(move down) (list 0 0))
                                   'drawing '() '() spec6))
  (println "--7--")
  (print-inst-list (translate-body (name-num-to-input-list '(move) (list 0))
                                   'drawing '(f) '() spec7))
  (println "--8--")
  (print-inst-list (translate-body (name-num-to-input-list '(move) (list 0))
                                   'drawing '(f) '(b) spec8))
  (println "--9--")
  (print-inst-list (translate-body (name-num-to-input-list '(move) (list 0))
                                   'drawing '() '(b) spec9))
  (println "--10--")
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec10))


  (define spec-scan1 '((move (bind _temp0 (f p)
                                   (bind _temp1 (g q)
                                         (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream ((mode (return mode))))))))))))))
  (define spec-scan1-i '((move (bind _temp0 (f p)
                                   (bind _temp1 (g q)
                                         (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream-initial ((mode (return mode))) a)))))))))))
  (define spec-scan1-s '((move (bind _temp0 (f p)
                                   (bind _temp1 (g q)
                                         (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream-seed ((mode (return mode))) a)))))))))))
  (println "---1---")
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan1))
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan1-i))
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan1-s))

  (define spec-scan2 '((move (bind _temp0 (f p)
                                   (bind _temp1 (g q)
                                         (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream ((mode (return drawing))))))))))))))
  (define spec-scan2-i '((move (bind _temp0 (f p)
                                   (bind _temp1 (g q)
                                         (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream-initial ((mode (return drawing))) a)))))))))))
  (define spec-scan2-s '((move (bind _temp0 (f p)
                                   (bind _temp1 (g q)
                                         (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream-seed ((mode (return drawing))) a)))))))))))
  (println "---2---")
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan2))
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan2-i))
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan2-s))

  (define spec-scan3 '((move (bind _temp0 (f p)
                                   (bind _temp1 (g q)
                                         (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream ((mode (if-else mode (return mode) (return a)))))))))))))))
  (define spec-scan3-i '((move (bind _temp0 (f p)
                                   (bind _temp1 (g q)
                                         (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream-initial ((mode (if-else mode (return mode) (return a)))) a)))))))))))
  (define spec-scan3-s '((move (bind _temp0 (f p)
                                   (bind _temp1 (g q)
                                         (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream-seed ((mode (if-else mode (return mode) (return a)))) a)))))))))))
  (println "---3---")
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan3))
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan3-i))
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan3-s))

  (define spec-scan4 '((move (bind _temp0 (f p)
                                   (bind _temp1 (g q)
                                         (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream ((mode (if-else mode (return mode) (return drawing)))))))))))))))
  (define spec-scan4-i '((move (bind _temp0 (f p)
                                     (bind _temp1 (g q)
                                           (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream-initial ((mode (if-else mode (return mode) (return drawing)))) a)))))))))))
  (define spec-scan4-s '((move (bind _temp0 (f p)
                                     (bind _temp1 (g q)
                                           (split ((a _temp0) (b _temp1) (c mode))
                                                (bind _temp2 (f a)
                                                      (if _temp2
                                                          (bind _temp3 (g b)
                                                                (if _temp3 (new-stream-seed ((mode (if-else mode (return mode) (return drawing)))) a)))))))))))
  (println "---4---")
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan4))
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan4-i))
  (print-inst-list (translate-body (name-num-to-input-list '(move mode q) (list 0 0 0))
                                   'drawing '(f g) '(p) spec-scan4-s))
  )
  
    