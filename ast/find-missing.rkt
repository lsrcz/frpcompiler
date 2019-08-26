#lang racket

(require "../util/util.rkt")
(require "syntax.rkt")
(require "nested-ref.rkt")

(provide
 find-missing-var
 find-missing-var-deep
 all-found
 not-found)

(struct all-found () #:transparent)
(struct not-found (intro) #:transparent)

(define (merge-missing missing-lst)
  (define (merge-two m1 m2)
    (match (list m1 m2)
      [(list (all-found) _) m2]
      [(list _ (all-found)) m1]
      [(list (not-found intro1) (not-found intro2))
       (not-found (remove-duplicates (append intro1 intro2)))]))
  (if (null? missing-lst)
      (all-found)
      (merge-two (car missing-lst) (merge-missing (cdr missing-lst)))))


(define (find-missing-var lst funclst return-val ref-list arg)
  (define (find-missing-var-inner lst arg)
    (let ([index (index-of lst arg)])
      (if index
          (all-found)
          (if (and (list? arg) (not (eq? (car arg) 'prev)))
              (merge-missing (cons (all-found)
                                   (for/list ([a arg])
                                     (find-missing-var-inner lst a))))
              (let ([removed (remove-prev arg)])
                (if (or (eq? return-val removed) (resolve-ref removed ref-list))
                    (all-found)
                    (not-found (list removed))))))))
  (match (find-missing-var-inner (append lst funclst) arg)
    [(all-found) (all-found)]
    [(not-found lst) (not-found (remove-duplicates lst))]))

(define (find-missing-var-deep lst funclst return-val ref-list inst)
  (let* ([missing-in-arg
          (if (custom? inst)
              (all-found)
              (find-missing-var lst funclst return-val ref-list
                                ((cond [(bind? inst) bind-body]
                                       [(if? inst) if-arg]
                                       [(if-else? inst) if-else-arg]
                                       [(return? inst) return-arg])
                                 inst)))]
         [new-lst (append (if (bind? inst) (list (bind-name inst)) '()) lst)]
         [missing-in-next-inst-first
          (if (not (return? inst))
              (find-missing-var-deep new-lst funclst return-val ref-list
                                     ((cond [(bind? inst) bind-inst]
                                            [(if? inst) if-branch]
                                            [(if-else? inst) if-else-then-branch]
                                            [(custom? inst) custom-body]
                                            [else "should not happen"])
                                      inst))
              (all-found))]
         [missing-in-next-inst-second
          (if (if-else? inst)
              (find-missing-var-deep new-lst funclst return-val ref-list (if-else-else-branch inst))
              (all-found))])
    (merge-missing (list missing-in-arg missing-in-next-inst-first missing-in-next-inst-second))))

(define (main)
  (println (find-missing-var '((g a)) '(f g) 'd '() '(f (g a))))
  (println (find-missing-var '() '(f g) 'd '() '(f (g (prev a)))))
  (println (find-missing-var-deep '() '(f g) 'd '() '(bind a (f t) (return a))))
  (println (find-missing-var-deep '() '(f g) 'd (list (nested-ref-table 't '())) '(bind a (f t) (return a))))
  (println (find-missing-var-deep '() '(f g) 'd (list (nested-ref-table 't '())) '(bind a (f t) (bind b (g s) (return a)))))
  (println (find-missing-var-deep '() '(f g) 'd (list (nested-ref-table 't '(s))) '(bind a (f t) (bind b (g s) (return a)))))
  )
