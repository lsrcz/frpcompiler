#lang racket

(require "../ir/inst.rkt")
(require "inst.rkt")
(require "../print/rxir.rkt")

(provide emit-rxir)

(define (emit-rxir ir-list-input)
  (define (emit-one ir rxir-mapping)
    (define (find-rxir ir)
      (cdr (assoc ir rxir-mapping)))
    (define (map-ref ref)
      (if (pair? ref)
          (rx-stream-pair-ref (find-rxir (car ref)) (cdr ref))
          (rx-stream-ref (find-rxir ref))))
    (define (emit-input ir)
      (match ir
        [(input-inst name num shape)
         (if (= num 0)
             (rx-name-ref name)
             (rx-pipe
              (rx-name-ref name)
              (append
               (for/list ([a (range num)])
                 (rx-start-with-undefined))
               (list (rx-buffer-count (+ 1 num) 1)))))]))
    (define (emit-intro ir)
      (match ir
        [(intro-inst intro-lst ref shape)
         (let ([computed-shape (cons (get-shape ref) (map get-shape intro-lst))])
           (rx-pipe
            (map-ref ref)
            (cons
             (rx-with-latest-from (map map-ref intro-lst))
             (if (not (equal? computed-shape shape))
                 (list (rx-map-flat computed-shape shape))
                 (list)))))]))
    (define (emit-compute ir)
      (match ir
        [(compute-inst to-compute name ref shape)
         (let [(shape (get-shape ref))]
           (rx-pipe
            (map-ref ref)
            (list
             (rx-map-shape (get-shape ref) ((if (list? shape) append cons) shape (list to-compute))))))]))
    (define (emit-filter ir)
      (match ir
        [(filter-inst arg ref shape)
         (rx-pipe
          (map-ref ref)
          (list
           (rx-filter (get-shape ref) arg)))]))
    (define (emit-partition ir)
      (match ir
        [(partition-inst arg ref shape)
         (rx-pipe
          (map-ref ref)
          (list
           (rx-partition (get-shape ref) arg)))]))
    (define (emit-ret ir)
      (match ir
        [(ret-inst arg ref)
         (rx-pipe
          (map-ref ref)
          (list
           (rx-map-shape (get-shape ref) arg)))]))
    (define (emit-merge ir)
      (match ir
        [(merge-inst to-merge)
         (rx-merge
          (map map-ref to-merge))]))
    (define (emit-merge-action ir)
      (match ir
        [(merge-action-inst to-merge)
         (rx-merge-action
          (map map-ref to-merge))]))
    (define (emit-ret-action ir)
      (match ir
        [(ret-action-inst return-val action ref)
         (rx-pipe
          (map-ref ref)
          (list
           (rx-ret-action
            (get-shape ref)
            return-val
            action)))]))
    (define (emit-custom ir)
      (match ir
        [(custom-inst name ref _)
         (rx-custom
          (map-ref ref)
          name)]))
    (define (emit-split ir)
      (match ir
        [(split-inst bindings body ref)
         (let* ([binding-ori (map cadr bindings)]
                [binding-new (map car bindings)]
                [switch-map-inst (rx-switch-map binding-new (emit-imperative body))])
           (rx-pipe
            (map-ref ref)
            (if (not (equal? (get-shape ref) binding-ori))
                (list (rx-map-shape (get-shape ref) binding-ori) switch-map-inst)
                (list switch-map-inst))))]))
    (define (emit-empty ir)
      (rx-empty))
    (define (emit-imperative inst)
      (match inst
        [(list 'bind name body next)
         (list 'bind name body (emit-imperative next))]
        [(list 'if arg branch)
         (list 'if arg (emit-imperative branch))]
        [(list 'if-else arg then-branch else-branch)
         (list 'if-else arg (emit-imperative then-branch) (emit-imperative else-branch))]
        [(ir-list _ _) (emit-rxir inst)]))
    ((cond [(input-inst? ir) emit-input]
           [(intro-inst? ir) emit-intro]
           [(compute-inst? ir) emit-compute]
           [(filter-inst? ir) emit-filter]
           [(partition-inst? ir) emit-partition]
           [(ret-inst? ir) emit-ret]
           [(merge-inst? ir) emit-merge]
           [(merge-action-inst? ir) emit-merge-action]
           [(ret-action-inst? ir) emit-ret-action]
           [(custom-inst? ir) emit-custom]
           [(split-inst? ir) emit-split]
           [(empty-inst? ir) emit-empty]
           [else (error "not-implemented")])
     ir))
  (define (iter ir-list-input rxir-mapping)
    (if (null? ir-list-input)
        rxir-mapping
        (let* ([cur (car ir-list-input)]
               [remaining (cdr ir-list-input)]
               [rxir (emit-one cur rxir-mapping)])
          (iter remaining (append rxir-mapping (list (cons cur rxir)))))))
  (match ir-list-input
    [(ir-list lst _) (rxir-list (map cdr (iter lst '())))]))

(define (main)
  (let* ([r1 (input-inst 'a 0 'a)]
         [r2 (input-inst 'b 1 '(b (prev b)))]
         [r3 (intro-inst (list r1) r2 '(b (prev b) a))]
         [r4 (compute-inst '(f b) 't r3 '(b (prev b) a t))]
         [r5 (filter-inst 't r4 '(b (prev b) a t))]
         [r6 (partition-inst 't r4 '(b (prev b) a t))]
         [r7 (ret-inst 't (cons r6 0))]
         [r8 (merge-inst (list r6 r7))]
         [r9 (merge-action-inst (list r6 r7))]
         [r10 (ret-action-inst 'x '(bind e (g x) (return e)) (cons r6 0))]
         [r11 (custom-inst 'custom r5 '(b (prev b) a t))]
         [r12 (split-inst '((x b)) (list 'bind 'e '(g x) (list 'if-else 'e (ir-list (list r1 r2 r3 r4) '()) (ir-list (list (empty-inst)) '()))) r11)])
    (print-rx-program '(a b)
     (emit-rxir
      (ir-list
       (list
        r1
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
        r12)
       '())))))
    