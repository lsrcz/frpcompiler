#lang racket

(require "syntax.rkt")
(require "../util/util.rkt")
(require "../test/test-spec.rkt")
(require "spec.rkt")

(provide extract extract-spec)                                                                                                                                                

(define (extract spec)
  (define temp-gen (get-temp-symbol-gen))
  (define (if-visitor arg branch)
    (if (list? arg)
        (let ([temp (temp-gen)])
          (build-bind temp arg (build-if temp branch)))
        (build-if arg branch)))
  (define bind-visitor build-bind)
  (define (if-else-visitor arg then-branch else-branch)
    (if (list? arg)
        (let ([temp (temp-gen)])
          (build-bind temp arg
                      (build-if-else temp then-branch else-branch)))
        (build-if-else arg then-branch else-branch)
        ))
  (define (return-visitor arg)
    (if (list? arg)
        (let ([temp (temp-gen)])
          (build-bind temp arg (build-return temp)))
        (build-return arg)))
  (define custom-visitor build-custom)
  (define (split-visitor bindings body)
    (define (iter bindings built-bindings-rev body)
      (if (null? bindings)
          (build-split (reverse built-bindings-rev) body)
          (let* ([cur (car bindings)]
                 [name (split-binding-name cur)]
                 [binding-body (split-binding-body cur)])
            (if (list? binding-body)
                (let ([temp (temp-gen)])
                  (build-bind temp binding-body
                              (iter (cdr bindings) (cons (list name temp) built-bindings-rev) body)))
                (iter (cdr bindings) (cons cur built-bindings-rev) body)))))
    (iter bindings '() body))
  (define new-stream-visitor build-new-stream)
  (define (new-stream-initial-visitor bodies initial)
    (if (list? initial)
        (let ([temp (temp-gen)])
          (build-bind temp initial
                      (build-new-stream-initial bodies temp)))
        (build-new-stream-initial bodies initial)))
  (define (new-stream-seed-visitor bodies seed)
    (if (list? seed)
        (let ([temp (temp-gen)])
          (build-bind temp seed
                      (build-new-stream-seed bodies temp)))
        (build-new-stream-seed bodies seed)))
  (define descend-list
    (list
     (cons 'if if-visitor)
     (cons 'if-else if-else-visitor)
     (cons 'return return-visitor)
     (cons 'bind bind-visitor)
     (cons 'custom custom-visitor)
     (cons 'split split-visitor)
     (cons 'new-stream new-stream-visitor)
     (cons 'new-stream-initial new-stream-initial-visitor)
     (cons 'new-stream-seed new-stream-seed-visitor)))

  (define extract-visitor
    (visitor
     descend-list
     '()
     descend-list
     '()
     ))
  (let ([name (car spec)]
        [body (cadr spec)])
    (list name (visit extract-visitor body #f))))

(define (extract-spec spec-input)
  (match spec-input
    [(spec inputs output funclist constantlist defaultval body)
     (spec inputs output funclist constantlist defaultval (map extract body))]))

(require "monad-desugar.rkt")

(define (main)
  (println (extract-spec (monad-desugar-spec drawing-spec)))
  (println (extract-spec (monad-desugar-spec drawing-split-spec))))
(main)


