#lang racket

(require "syntax.rkt")
(require "../test/test-spec.rkt")
(provide monad-desugar monad-desugar-spec)

(define (monad-desugar spec)
  (define (desugar-body body)
    (cond [(if? body)
           (desugar-if body)]
          [(if-else? body)
           (desugar-if-else body)]
          [(begin? body)
           (desugar-begin body)]
          [(let? body)
           (error "should not happen")]
          [(custom? body)
           (desugar-custom body)]
          [else body]))
  (define (desugar-if body)
    (let ([arg (if-arg body)]
          [desugared-branch (desugar-body (if-branch body))])
      (build-if arg desugared-branch)))
  (define (desugar-if-else body)
    (let ([arg (if-else-arg body)]
          [desugared-then (desugar-body (if-else-then-branch body))]
          [desugared-else (desugar-body (if-else-else-branch body))])
      (build-if-else arg desugared-then desugared-else)))
  (define (desugar-begin body)
    (define (desugar-seq body)
      (if (null? (cdr body))
          (if (let? (car body))
              (error "wrong format")
              (desugar-body (car body)))
          (if (let? (car body))
              (let ([let-stmt (car body)])
                (build-bind (let-name let-stmt) (let-body let-stmt) (desugar-seq (cdr body))))
              (error "wrong format"))))
    (desugar-seq (begin-seq body)))
  (define (desugar-custom body)
    (let ([name (custom-name body)]
          [desugared-inst (desugar-body (custom-body body))])
      (build-custom name desugared-inst)))
  (let ([name (car spec)]
        [body (cadr spec)])
    (list name (desugar-body body))))

(define (monad-desugar-spec spec-input)
  (match spec-input
    [(spec inputs output funclist body)
     (spec inputs output funclist (map monad-desugar body))]))

(define (main)
  (println (monad-desugar
            '(mode
              (begin
                (let _temp2 (f mode))
                (if-else _temp2
                         (begin
                           (let _temp0 (h))
                           (return _temp0))
                         (begin (let _temp1 (g))
                                (return _temp1)))))))
  (println (monad-desugar
            '(move
              (begin
                (let _temp3 (f mode))
                (if _temp3
                    (begin
                      (let _temp2 (m drawing))
                      (if-else _temp2
                               (begin
                                 (let _temp0 (l down move))
                                 (return _temp0))
                               (begin
                                 (let _temp1 (n drawing (prev move) move))
                                 (return _temp1)))))))))
  (println (monad-desugar
            '(move
              (begin
                (let _temp3 (f mode))
                (if _temp3
                    (begin
                      (let _temp2 (m drawing))
                      (if-else _temp2
                               (begin
                                 (let _temp0 (l down move))
                                 (return _temp0))
                               (begin
                                 (let _temp1 (n drawing (prev move) move))
                                 (return _temp1))))))))))


              
          
    