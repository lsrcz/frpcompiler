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
          [(begin? body)
           (expand-begin body)]
          [(return? body)
           (expand-return body)]
          [(custom? body)
           (expand-custom body)]
          [(let? body)
           body]))
  (define (expand-begin body)
    (let ([seq (begin-seq body)])
      (build-begin (map expand-body seq))))                                                                                                                                                             
  (define (expand-if body)
    (let ([arg (if-arg body)]
          [expanded-branch (expand-body (if-branch body))])
      (if (list? arg)
          (let ([temp (temp-gen)])
            (build-begin
             (list
              (build-let temp arg)
              (build-if temp expanded-branch))))
          (build-if arg expanded-branch))))
  (define (expand-if-else body)
    (let ([arg (if-else-arg body)]
          [expanded-then (expand-body (if-else-then-branch body))]
          [expanded-else (expand-body (if-else-else-branch body))])
      (if (list? arg)
          (let ([temp (temp-gen)])
            (build-begin
             (list
              (build-let temp arg)
              (build-if-else temp expanded-then expanded-else))))
          (build-if-else arg expanded-then expanded-else)
          )))
  (define (expand-return body)
    (let ([arg (return-arg body)])
      (if (list? arg)
          (let ([temp (temp-gen)])
            (build-begin
             (list
              (build-let temp arg)
              (build-return temp))))
          body)))
  (define (expand-custom body)
    (let ([name (custom-name body)])
      (build-custom name (expand-body (custom-body body)))))
  (let ([name (car spec)]
        [body (cadr spec)])
    (list name (expand-body body))))

(define (expand-spec spec-input)
  (match spec-input
    [(spec inputs output funclist body)
     (spec inputs output funclist (map expand body))]))

(define (main)
  (println (expand (car (spec-body drawing-spec))))
  (println (expand (cadr (spec-body drawing-spec))))
  (println (expand (cadr (spec-body drawing-modified-spec))))
  (println (expand-spec drawing-spec)))


