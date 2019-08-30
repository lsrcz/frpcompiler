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
(struct not-found (intro intro-const) #:transparent)

(define (merge-missing missing-lst)
  (define (merge-two m1 m2)
    (match (list m1 m2)
      [(list (all-found) _) m2]
      [(list _ (all-found)) m1]
      [(list (not-found intro1 intro-const1) (not-found intro2 intro-const2))
       (not-found (remove-duplicates (append intro1 intro2))
                  (remove-duplicates (append intro-const1 intro-const2)))]))
  (if (null? missing-lst)
      (all-found)
      (merge-two (car missing-lst) (merge-missing (cdr missing-lst)))))


(define (find-missing-var shape funclst return-val ref-list arg)
  (define (find-missing-var-inner available-list arg)
    (let ([index (index-of available-list arg)])
      (if index
          (all-found)
          (if (and (list? arg) (not (eq? (car arg) 'prev)))
              (merge-missing (cons (all-found)
                                   (for/list ([a arg])
                                     (find-missing-var-inner available-list a))))
              (let ([removed (remove-prev arg)])
                (if (eq? return-val removed)
                    (all-found)
                    (if (resolve-ref removed ref-list)
                        (not-found '() (list removed))
                        (not-found (list removed) '()))))))))
  (match (find-missing-var-inner (append shape funclst) arg)
    [(all-found) (all-found)]
    [(not-found lst clist) (not-found (remove-duplicates lst) (remove-duplicates clist))]))

(define (find-missing-var-deep shape funclst return-val ref-list inst)
  (let* ([missing-in-arg
          (if (custom? inst)
              (all-found)
              (find-missing-var shape funclst return-val ref-list
                                ((cond [(bind? inst) bind-body]
                                       [(if? inst) if-arg]
                                       [(if-else? inst) if-else-arg]
                                       [(return? inst) return-arg])
                                 inst)))]
         [new-shape (append (if (bind? inst) (list (bind-name inst)) '()) shape)]
         [missing-in-next-inst-first
          (if (not (return? inst))
              (find-missing-var-deep new-shape funclst return-val ref-list
                                     ((cond [(bind? inst) bind-inst]
                                            [(if? inst) if-branch]
                                            [(if-else? inst) if-else-then-branch]
                                            [(custom? inst) custom-body]
                                            [else "should not happen"])
                                      inst))
              (all-found))]
         [missing-in-next-inst-second
          (if (if-else? inst)
              (find-missing-var-deep new-shape funclst return-val ref-list (if-else-else-branch inst))
              (all-found))])
    (merge-missing (list missing-in-arg missing-in-next-inst-first missing-in-next-inst-second))))

(define (main)
  (println (find-missing-var '((g a)) '(f g) 'd '() '(f (g a))))
  (println (find-missing-var '() '(f g) 'd '() '(f (g (prev a)))))
  (println (find-missing-var-deep '() '(f g) 'd '() '(bind a (f t) (return a))))
  (println (find-missing-var-deep '() '(f g) 'd '(t) '(bind a (f t) (return a))))
  (println (find-missing-var-deep '() '(f g) 'd '(t) '(bind a (f t) (bind b (g s) (return a)))))
  (println (find-missing-var-deep '() '(f g) 'd '(t s) '(bind a (f t) (bind b (g s) (return a)))))
  )
