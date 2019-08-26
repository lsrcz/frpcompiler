#lang racket

(require "syntax.rkt")
(require "../util/util.rkt")
(require "../test/test-spec.rkt")

(provide expand expand-spec)                                                                                                                                                
                                                                                                                                                                         
(define (expand spec)
  (define temp-gen (get-temp-symbol-gen))
  (define (expand-body body)
    (cond [(if? body)
           (expand-if body)]
          [(if-else? body)
           (expand-if-else body)]
          [(return? body)
           (expand-return body)]
          [(custom? body)
           (expand-custom body)]
          [(split? body)
           (expand-split body)]
          [(new-stream? body)
           (expand-new-stream body)]
          [(empty-stream? body)
           body]
          [(bind? body)
           (expand-bind body)]))
  (define (expand-if body)
    (let ([arg (if-arg body)]
          [expanded-branch (expand-body (if-branch body))])
      (if (list? arg)
          (let ([temp (temp-gen)])
            (build-bind temp arg
                        (build-if temp expanded-branch)))
          (build-if arg expanded-branch))))
  (define (expand-bind body)
    (build-bind (bind-name body) (bind-body body) (bind-inst body)))
  (define (expand-if-else body)
    (let ([arg (if-else-arg body)]
          [expanded-then (expand-body (if-else-then-branch body))]
          [expanded-else (expand-body (if-else-else-branch body))])
      (if (list? arg)
          (let ([temp (temp-gen)])
            (build-bind temp arg
                        (build-if-else temp expanded-then expanded-else)))
          (build-if-else arg expanded-then expanded-else)
          )))
  (define (expand-return body)
    (let ([arg (return-arg body)])
      (if (list? arg)
          (let ([temp (temp-gen)])
            (build-bind temp arg
                        (build-return temp)))
          body)))
  (define (expand-custom body)
    (let ([name (custom-name body)])
      (build-custom name (expand-body (custom-body body)))))
  (define (expand-split body)
    (define (iter bindings-agg bindings-sub built-bindings-sub-rev body)
      (if (null? bindings-sub)
          (build-split (cons bindings-agg (reverse built-bindings-sub-rev)) (expand-body body))
          (let* ([cur (car bindings-sub)]
                 [name (split-binding-name cur)]
                 [binding-body (split-binding-body cur)])
            (if (list? binding-body)
                (let ([temp (temp-gen)])
                  (build-bind temp binding-body
                              (iter bindings-agg (cdr bindings-sub) (cons (list name temp) built-bindings-sub-rev) body)))
                (iter bindings-agg (cdr bindings-sub) (cons cur built-bindings-sub-rev) body)))))
    (let* ([bindings (split-bindings body)]
           [bindings-agg (split-bindings-agg bindings)]
           [bindings-sub (split-bindings-sub bindings)]
           [body (split-body body)])
      (iter bindings-agg bindings-sub '() body)))
  (define (expand-new-stream body)
    (let ([expanded-body (map expand (new-stream-body body))])
      (build-new-stream expanded-body)))
  (let ([name (car spec)]
        [body (cadr spec)])
    (list name (expand-body body))))

(define (expand-spec spec-input)
  (match spec-input
    [(spec inputs output funclist constantlist body)
     (spec inputs output funclist constantlist (map expand body))]))

(require "monad-desugar.rkt")

(define (main)
  (println (expand-spec (monad-desugar-spec drawing-spec)))
  (println (expand-spec (monad-desugar-spec drawing-split-spec))))


