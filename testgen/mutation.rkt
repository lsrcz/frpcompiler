#lang rosette/safe

(require "sym-trace.rkt"
         "../interpret/analyzed.rkt"
         "../interpret/environment.rkt"
         "../interpret/spec.rkt"
         "../test/test-spec.rkt")

(define (find-distinguish-input spec1 spec2 binding sym-trace)
  (time (synthesize
         #:forall (list)
         #:guarantee
         (assert (not
                  (equal? (interpret-spec spec1 sym-trace binding)
                          (interpret-spec spec2 sym-trace binding)))))))

(define (main)
   (define (test-case1)
     (define spec1-input (spec '(a b) 'd '(add1 + - not >) '(t x)
                       '((a (split ((a1 (add1 a)))
                                   (if (> a1 t)
                                       (new-stream ((b (if-else (undefined? d) (return (add1 (+ a1 b))) (return (+ b d))))))))))))
     (define spec2-input (spec '(a b) 'd '(add1 + - not >) '(t x)
                               '((a (split ((a1 (add1 a)))
                                           (if (> a1 t)
                                               (new-stream ((b (if (undefined? d) (return (add1 (+ a1 b)))))))))))))
    
     (define binding-input (list
                            (binding 'add1 add1)
                            (binding '+ +)
                            (binding '- -)
                            (binding 'not not)
                            (binding 'undefined? undefined?)
                            (binding '> >)
                            (binding 't 1)
                            (binding 'x 1)
                            (binding 'undefined 'undefined)))

     (define sym-trace (get-symbolic-trace
                        (list (cons 'a integer-constructor)
                              (cons 'b integer-constructor))
                        5))

     (define m (find-distinguish-input spec1-input spec2-input
                                       binding-input sym-trace ))
     (define solved-trace (evaluate sym-trace m))
     (displayln solved-trace)
     (displayln (interpret-spec spec1-input solved-trace binding-input))
     (displayln (interpret-spec spec2-input solved-trace binding-input))
     
     )
  (test-case1)
  )
(main)