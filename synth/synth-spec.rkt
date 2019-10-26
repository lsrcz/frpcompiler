#lang rosette
(error-print-width 100000)

(require "../interpret/interpret-spec/spec.rkt")
(require "../interpret/interpret-spec/environment.rkt")
(require "../interpret/subscribe-fsm.rkt")
(require "../ast/spec.rkt")
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

(define concrete-all-bindings
  (append non-symbolic-bindings
          (list
           (binding 'f1 f1)
           (binding 'f2 f2))))


(define-symbolic f1s (~> boolean? boolean? boolean? boolean? integer?))
(define-symbolic f2s (~> boolean? boolean? boolean? boolean? integer?))

(define symbolic-bindings
          (list
           (list (binding 'f1 f1s) 4 3)
           (list (binding 'f2 f2s) 4 3)))

(define symbolic-all-bindings
  (append non-symbolic-bindings
          (map car symbolic-bindings)))


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
(define constructor-list (list
                          (cons 'clock clock-constructor)
                          (cons 'motion motion-constructor)))
(define sym-trace (get-symbolic-trace constructor-list 8))

#|
(f1c b1 b2 b3 b4)
(f1c1 b1 b2 b3 b4)
(evaluate sym-trace m1)
(evaluate (interpret-spec sprinkler-spec sym-trace bindings) m1)
(evaluate (interpret-spec sprinkler-spec sym-trace symbolic-bindings) m1)
(car '())
|#

(define (synthesize-spec spec symbolic-bindings
                         concrete-bindings sym-cons-list max-length init-trace init-result annotate-func)
  (define bindings (append (map car symbolic-bindings) concrete-bindings))
  (define sym-trace (get-symbolic-trace sym-cons-list 1))
    (define (build-symbolic-constraints symbolic-bindings)
    (define (build-symbolic-list num)
      (if (= num 0)
          '()
          (begin
            (define-symbolic* x boolean?)
            (cons x (build-symbolic-list (- num 1))))))
    (define (build-one symbolic-binding)
      (let ([f (binding-value (car symbolic-binding))]
            [arity (cadr symbolic-binding)]
            [max (caddr symbolic-binding)])
        (define l (build-symbolic-list arity))
        (cons l (and (>= (apply f l) 0) (< (apply f l) max)))))
    (if (null? symbolic-bindings)
        (cons (list) #t)
        (let* ([nxt (build-symbolic-constraints (cdr symbolic-bindings))]
               [nxt-lst (car nxt)]
               [nxt-constraint (cdr nxt)]
               [this (build-one (car symbolic-bindings))]
               [this-lst (car this)]
               [this-constraint (cdr this)])
          (cons (append nxt-lst this-lst) (and nxt-constraint this-constraint)))))
  (define symbolic-constraint (build-symbolic-constraints symbolic-bindings))
  (define (iter inout last-model cur-length)
    (if (> cur-length max-length)
        last-model
        (begin
                          (displayln "---------")
          (define sym-trace (get-symbolic-trace sym-cons-list cur-length))

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
          (define concrete-bindings (evaluate bindings last-model))
          (define (distinguish-constraint)
           (assert (not (equal? (interpret-spec spec sym-trace concrete-bindings)
                        (interpret-spec spec sym-trace bindings)))))
          (define new-model (time (synthesize #:forall '()
                                              #:guarantee (begin
                                                            (ground-truth-constraint inout)
                                                            (distinguish-constraint)
                                                            ))))
          ;(displayln inout)
          
          (if (unsat? new-model)
              (iter inout last-model (+ 1 cur-length))
              (let* ([complete-model (complete-solution new-model (symbolics (interpret-spec spec sym-trace bindings)))]
                     [in (evaluate sym-trace complete-model)]
                     [out (annotate-func in)]
                     [newinout (cons (cons in out) inout)])

                (displayln "NEW TRACE")
                (display "IN: ")
                (displayln in)
                (display "GROUND TRUTH:   ")
                (displayln out)
                
                (define new-model (time (synthesize #:forall (car symbolic-constraint)
                                                    #:guarantee (begin
                                                                  (ground-truth-constraint newinout)
                                                                  (assert (cdr symbolic-constraint))
                                                                  ))))
                ;(displayln (evaluate  (interpret-spec spec in bindings) complete-model))
                (iter newinout new-model cur-length))))))
  (let* ([initial-model (complete-solution
                         (synthesize #:forall '()
                                     #:guarantee (assert (equal? init-result (interpret-spec spec init-trace bindings))))
                         (symbolics bindings))])
    (iter (list (cons init-trace init-result)) initial-model 1)))

(define (annotate-func in)
  (displayln in)
  (let ([lst (read)])
    (map (lambda (x) (if (eq? x 'no-evt) (empty-event) (event 'sprinkler x))) lst)))

(define (annotate-groundtruth input-spec input-bindings)
  (define (annotate-func in)
    (interpret-spec input-spec in input-bindings))
  annotate-func)

(define synth-m (synthesize-spec sprinkler-spec symbolic-bindings non-symbolic-bindings constructor-list 5 (trace (list)) (list)
                                 (annotate-groundtruth sprinkler-spec concrete-all-bindings)))

(verify (eq? (interpret-spec sprinkler-spec sym-trace (evaluate symbolic-all-bindings synth-m))
             (interpret-spec sprinkler-spec sym-trace concrete-all-bindings)))

(define poster-trace
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

(define synth-m1 (synthesize-spec sprinkler-spec symbolic-bindings non-symbolic-bindings constructor-list 5 poster-trace
                                  (interpret-spec sprinkler-spec poster-trace concrete-all-bindings)
                                 (annotate-groundtruth sprinkler-spec concrete-all-bindings)))

(verify (eq? (interpret-spec sprinkler-spec sym-trace (evaluate symbolic-all-bindings synth-m))
             (interpret-spec sprinkler-spec sym-trace concrete-all-bindings)))




                         

