#lang racket

(require "syntax.rkt")
(require "../test/test-spec.rkt")
(require "spec.rkt")
(provide monad-desugar monad-desugar-spec)

(define (monad-desugar spec)
  (define (begin-visitor . seq)
    (define (desugar-seq body)
      (if (null? (cdr body))
          (if (let? (car body))
              (error "wrong format")
              (car body))
          (if (let? (car body))
              (let ([let-stmt (car body)])
                (build-bind (let-name let-stmt) (let-body let-stmt) (desugar-seq (cdr body))))
              (error "wrong format"))))
    (desugar-seq seq))
  (define descend-list
    (list
     (cons 'if build-if)
     (cons 'if-else build-if-else)
     (cons 'bind build-bind)
     (cons 'custom build-custom)
     (cons 'split build-split)
     (cons 'new-stream build-new-stream)
     (cons 'new-stream-initial build-new-stream-initial)
     (cons 'new-stream-seed build-new-stream-seed)
     (cons 'begin begin-visitor)))
  (define desugar-visitor
    (visitor
     descend-list
     '()
     descend-list
     '()))
  (let ([name (car spec)]
        [body (cadr spec)])
    (list name (visit desugar-visitor body #f))))

(define (monad-desugar-spec spec-input)
  (match spec-input
    [(spec inputs output funclist constantlist defaultval body)
     (spec inputs output funclist constantlist defaultval (map monad-desugar body))]))

(module+ test
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
                                 (return _temp1)))))))))
  (newline)
  (println (monad-desugar
            '(mode
              (begin
                (let down-snap down)
                (split (e (mode-snapshot mode)
                          (down-snapshot down-snap))
                       (begin
                         (let x (f mode-snapshot))
                         (if-else x
                                  (new-stream
                                   ((move
                                     (begin
                                       (let d (g drawing))
                                       (if-else d
                                                (return (l down-snapshot move))
                                                (return (n drawing (prev move) move)))))))
                                  (empty-stream))))))))
  (println (monad-desugar-spec drawing-spec))
  )


              
          
    