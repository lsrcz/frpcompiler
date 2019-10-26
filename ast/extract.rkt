#lang racket

(require "syntax.rkt")
(require "../util/util.rkt")
(require "../test/test-spec.rkt")
(require "spec.rkt")

(provide extract extract-spec)                                                                                                                                                
                                                                                                                                                                         
(define (extract spec)
  (define temp-gen (get-temp-symbol-gen))
  (define (extract-body body)
    (cond [(if? body)
           (extract-if body)]
          [(if-else? body)
           (extract-if-else body)]
          [(return? body)
           (extract-return body)]
          [(custom? body)
           (extract-custom body)]
          [(split? body)
           (extract-split body)]
          [(new-stream? body)
           (extract-new-stream body)]
          [(new-stream-initial? body)
           (extract-new-stream-initial body)]
          [(new-stream-seed? body)
           (extract-new-stream-seed body)]
          [(empty-stream? body)
           body]
          [(bind? body)
           (extract-bind body)]))
  (define (extract-if body)
    (let ([arg (if-arg body)]
          [extracted-branch (extract-body (if-branch body))])
      (if (list? arg)
          (let ([temp (temp-gen)])
            (build-bind temp arg
                        (build-if temp extracted-branch)))
          (build-if arg extracted-branch))))
  (define (extract-bind body)
    (build-bind (bind-name body) (bind-body body) (extract-body (bind-inst body))))
  (define (extract-if-else body)
    (let ([arg (if-else-arg body)]
          [extracted-then (extract-body (if-else-then-branch body))]
          [extracted-else (extract-body (if-else-else-branch body))])
      (if (list? arg)
          (let ([temp (temp-gen)])
            (build-bind temp arg
                        (build-if-else temp extracted-then extracted-else)))
          (build-if-else arg extracted-then extracted-else)
          )))
  (define (extract-return body)
    (let ([arg (return-arg body)])
      (if (list? arg)
          (let ([temp (temp-gen)])
            (build-bind temp arg
                        (build-return temp)))
          body)))
  (define (extract-custom body)
    (let ([name (custom-name body)])
      (build-custom name (extract-body (custom-body body)))))
  (define (extract-split body)
    (define (iter bindings built-bindings-rev body)
      (if (null? bindings)
          (build-split (reverse built-bindings-rev) (extract-body body))
          (let* ([cur (car bindings)]
                 [name (split-binding-name cur)]
                 [binding-body (split-binding-body cur)])
            (if (list? binding-body)
                (let ([temp (temp-gen)])
                  (build-bind temp binding-body
                              (iter (cdr bindings) (cons (list name temp) built-bindings-rev) body)))
                (iter (cdr bindings) (cons cur built-bindings-rev) body)))))
    (let* ([bindings (split-bindings body)]
           [body (split-body body)])
      (iter bindings '() body)))
  (define (extract-new-stream body)
    (let ([extracted-body (map extract (new-stream-body body))])
          (build-new-stream extracted-body)))
  (define (extract-new-stream-initial body)
    (let ([extracted-body (map extract (new-stream-initial-body body))]
          [initial (new-stream-initial-initial body)])
      (if (list? initial)
          (let ([temp (temp-gen)])
            (build-bind temp initial
                        (build-new-stream-initial extracted-body temp)))
          (build-new-stream-initial extracted-body initial))))
  (define (extract-new-stream-seed body)
    (let ([extracted-body (map extract (new-stream-seed-body body))]
          [seed (new-stream-seed-seed body)])
      (if (list? seed)
          (let ([temp (temp-gen)])
            (build-bind temp seed
                        (build-new-stream-seed extracted-body temp)))
          (build-new-stream-seed extracted-body seed))))
  (let ([name (car spec)]
        [body (cadr spec)])
    (list name (extract-body body))))

(define (extract-spec spec-input)
  (match spec-input
    [(spec inputs output funclist constantlist defaultval body)
     (spec inputs output funclist constantlist defaultval (map extract body))]))

(require "monad-desugar.rkt")

(define (main)
  (println (extract-spec (monad-desugar-spec drawing-spec)))
  (println (extract-spec (monad-desugar-spec drawing-split-spec))))


