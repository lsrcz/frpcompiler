#lang rosette/safe

(require "../test/test-spec.rkt")
(require "../interpret/interpret-spec/spec.rkt")
(require "../interpret/interpret-spec/environment.rkt")
(require "../interpret/subscribe-fsm.rkt")
(require rosette/lib/angelic)

(define sprinkler-spec
  (spec
   '(clock motion)
   'sprinkler
   '(eq? undefined?)
   '(six sixten detected not-detected on off f1 f2)
   '((clock six) (motion not-detected))
   '((clock (case-multi ((eq? clock six)
                         (eq? clock sixten)
                         (and (eq? motion detected) (eq? (prev motion) not-detected))
                         (and (eq? motion not-detected) (eq? (prev motion) detected)))
                        ((return on)
                         (return off)
                         (return-empty))
                        f1))
     (motion (case-multi ((eq? clock six)
                          (eq? clock sixten)
                          (and (eq? motion detected) (eq? (prev motion) not-detected))
                          (and (eq? motion not-detected) (eq? (prev motion) detected)))
                         ((return on)
                          (return off)
                          (return-empty))
                         f2)))))

(define six 'six)
(define sixten 'sixten)
(define detected 'detected)
(define not-detected 'not-detected)
(define on 'on)
(define off 'off)


(define (f1 b1 b2 b3 b4)
  (cond [b1 0]
        [b2 1]
        [else 2]))
(define (f2 b1 b2 b3 b4)
  (cond [(and b4 b1) 0]
        [b3 1]
        [else 2]))

(define (and-func a b)
  (and a b))

(define non-symbolic-bindings
    (list
   (binding 'eq? eq?)
   (binding 'six six)
   (binding 'sixten sixten)
   (binding 'detected detected)
   (binding 'not-detected not-detected)
   (binding 'undefined? undefined?)
   (binding 'not not)
   (binding 'and and-func)
   (binding 'on on)
   (binding 'off off)))

(define bindings
  (append non-symbolic-bindings
          (list
           (binding 'f1 f1)
           (binding 'f2 f2))))


(define-symbolic f1s (~> boolean? boolean? boolean? boolean? integer?))
(define-symbolic f2s (~> boolean? boolean? boolean? boolean? integer?))

(define symbolic-bindings
  (append non-symbolic-bindings
          (list
           (binding 'f1 f1s)
           (binding 'f2 f2s))))

(define trace-input
  (trace
   (list
    (event 'clock six)
    (event 'motion not-detected)
    (empty-event)
    (event 'motion detected)
    (empty-event)
    (event 'motion not-detected)
    (event 'clock sixten)
    (event 'motion not-detected))))
(define trace-result
  (list (event 'sprinkler on)
        (empty-event)
        (empty-event)
        (event 'sprinkler 'off)
        (empty-event)
        (event 'sprinkler 'on)
        (event 'sprinkler 'off)
        (empty-event)))

(define stream-body-for-testing-analyze (spec-body sprinkler-spec))

(eq? trace-result (interpret-spec sprinkler-spec trace-input bindings))




(define m (time (synthesize #:forall '()
            #:guarantee (assert (equal? (interpret-spec sprinkler-spec trace-input symbolic-bindings)
                                        trace-result)))))



(define (get-single-symbolic-event name constructor)
  (event name (constructor)))

(define (get-symbolic-event constructor-mapping)
  (if (null? constructor-mapping)
      (empty-event)
      (let* ([cur (car constructor-mapping)]
             [rest (cdr constructor-mapping)]
             [name (car cur)]
             [constructor (cdr cur)])
        (choose* (get-single-symbolic-event name constructor) (get-symbolic-event rest)))))

(define (get-symbolic-event-list constructor-mapping length)
  (define (iter num)
    (if (= num 0) '()
        (cons (get-symbolic-event constructor-mapping) (iter (- num 1)))))
  (iter length))

(define (get-symbolic-trace constructor-mapping length)
  (trace (get-symbolic-event-list constructor-mapping length)))

(define (clock-constructor) (choose* 'six 'sixten))
(define (motion-constructor) (choose* 'detected 'not-detected))
(define sym-trace (get-symbolic-trace (list
                                       (cons 'clock clock-constructor)
                                       (cons 'motion motion-constructor)) 8))

(define model (synthesize #:forall '()
                          #:guarantee (assert (equal? trace-input sym-trace))))

(define m1 (time (synthesize #:forall '()
                       #:guarantee (begin
                                     (assert (not (equal? (interpret-spec sprinkler-spec sym-trace (evaluate symbolic-bindings m))
                                                          (interpret-spec sprinkler-spec sym-trace symbolic-bindings))))
                                     (assert (equal? (interpret-spec sprinkler-spec trace-input symbolic-bindings) trace-result))))))

(define f1c (evaluate f1s m))
(define f1c1 (evaluate f1s m1))
(define-symbolic b1 boolean?)
(define-symbolic b2 boolean?)
(define-symbolic b3 boolean?)
(define-symbolic b4 boolean?)
#|
(f1c b1 b2 b3 b4)
(f1c1 b1 b2 b3 b4)
(evaluate sym-trace m1)
(evaluate (interpret-spec sprinkler-spec sym-trace bindings) m1)
(evaluate (interpret-spec sprinkler-spec sym-trace symbolic-bindings) m1)
(car '())
|#

(define (synthesize-spec spec bindings sym-trace init-trace init-result annotate-func)
  (define (iter inout last-model)
    (define (ground-truth-constraint inout)
      (if (null? inout)
          (void)
          (let* ([cur (car inout)]
                 [i (car cur)]
                 [o (cdr cur)]
                 [rest (cdr inout)])
            (begin
              (assert (equal? (interpret-spec spec i bindings) o))
              (ground-truth-constraint rest)))))
    (define (distinguish-constraint)
      (assert (not (equal? (interpret-spec spec sym-trace (evaluate bindings last-model))
                           (interpret-spec spec sym-trace bindings)))))
    (define new-model (time (synthesize #:forall '()
                                        #:guarantee (begin
                                                      (ground-truth-constraint inout)
                                                      (distinguish-constraint)
                                                      ))))
    (if (unsat? new-model)
        last-model
        (let* ([complete-model (complete-solution new-model (symbolics (cons sym-trace bindings)))]
               [in (evaluate sym-trace complete-model)]
               [out (annotate-func in)])
          (displayln "NEW TRACE")
          (displayln in)
          (displayln out)
          (displayln (evaluate (interpret-spec sprinkler-spec sym-trace bindings) complete-model))
          (iter (cons (cons in out) inout) complete-model))))
  (let* ([initial-model (complete-solution
                         (synthesize #:forall '()
                                     #:guarantee (assert (equal? init-result (interpret-spec spec init-trace bindings))))
                         (symbolics (cons sym-trace bindings)))])
    (iter (list (cons init-trace init-result)) initial-model)))

(define (annotate-func in)
  (displayln in)
  (let ([lst (read)])
    (map (lambda (x) (if (eq? x 'no-evt) (empty-event) (event 'sprinkler x))) lst)))

(define (annotate-groundtruth input-spec input-bindings)
  (define (annotate-func in)
    (interpret-spec input-spec in input-bindings))
  annotate-func)

(define synth-m (synthesize-spec sprinkler-spec symbolic-bindings sym-trace (trace (list)) (list)
                                 (annotate-groundtruth sprinkler-spec bindings)))

(verify (eq? (interpret-spec sprinkler-spec sym-trace (evaluate symbolic-bindings synth-m))
             (interpret-spec sprinkler-spec sym-trace bindings)))




                                         


                         

