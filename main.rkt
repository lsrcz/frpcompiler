#lang racket

(require "ast/extract.rkt")
(require "ast/monad-desugar.rkt")
(require "test/test-spec.rkt")
(require "ast/spec.rkt")
(require "print/rxir.rkt")
(require "compile.rkt")

(define (main)
  (let ([spec1
         (spec
          '(mode move down)
          'drawing
          '(f g)
          '()
          '()
          '((mode
             (begin
               (let t (g mode))
               (if-else (f t)
                        (if down
                            (return t))
                        (if-else down (return down) (return move)))))
           (move
             (begin
               (let x (g (prev mode)))
               (if x (return x))))))]
        [spec2
         (spec
          '(mode move down)
          'drawing
          '(f g)
          '()
          '()
          '((move
             (begin
               (let x (g (prev mode)))
               (if x (return x))))))]
        [spec3
         (spec
          '(mode move down)
          'drawing
          '(f g)
          '(ready)
          '((mode ready))
          '((move
             (begin
               (let x (g (prev mode)))
               (if x (return x))))))])
    (println (monad-desugar-spec spec1))
    (println (extract-spec (monad-desugar-spec spec1)))
    (print-rx-program (compile spec1))
    (print-rx-program (compile spec2))
    (print-rx-program (compile spec3)))
  (print-rx-program (compile drawing-spec))
  (print-rx-program (compile drawing-custom-spec))
  (print-rx-program (compile drawing-split-spec))

  )

(define (test-drawing) (print-rx-program (compile drawing-spec)))

(define (test-action)
  (let ([spec1
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if-else (f mode-snapshot)
                             (new-stream
                              ((move (if-else (g drawing)
                                              (return (l down-snapshot move))
                                              (return (n drawing (prev move) move))))))
                             (empty-stream))))
            (down (if (g drawing) (return down)))))])
    (print-rx-program (compile spec1))))

(define (test-new-stream)
  (let ([spec-no-act-no-init-one
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream
                         ((move (return move)))))))))]
        [spec-act-no-init-one
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream
                         ((move (return drawing)))))))))]
        [spec-no-act-no-init-mul
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream
                         ((move (if-else down-snapshot (return move) (return down-snapshot))))))))))]
        [spec-act-no-init-mul
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream
                         ((move (if-else down-snapshot (return move) (return drawing))))))))))]
        [spec-no-act-init-one
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream-initial
                         ((move (return move)))
                         (g down-snapshot)))))))]
        [spec-act-init-one
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream-initial
                         ((move (return drawing)))
                         (g down-snapshot)))))))]
        [spec-no-act-init-mul
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream-initial
                         ((move (if-else down-snapshot (return move) (return down-snapshot))))
                         (g down-snapshot)))))))]
        [spec-act-init-mul
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream-initial
                         ((move (if-else down-snapshot (return move) (return drawing))))
                         (g down-snapshot)))))))]
        [spec-no-act-seed-one
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream-seed
                         ((move (return move)))
                         (g down-snapshot)))))))]
        [spec-act-seed-one
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream-seed
                         ((move (return drawing)))
                         (g down-snapshot)))))))]
        [spec-no-act-seed-mul
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream-seed
                         ((move (if-else down-snapshot (return move) (return down-snapshot))))
                         (g down-snapshot)))))))]
        [spec-act-seed-mul
         (spec
          '(mode down move)
          'drawing
          '(f g l n)
          '()
          '()
          '((mode
             (split ((mode-snapshot mode)
                     (down-snapshot down))
                    (if (f mode-snapshot)
                        (new-stream-seed
                         ((move (if-else down-snapshot (return move) (return drawing))))
                         (g down-snapshot)))))))])
    (print-rx-program (compile spec-no-act-no-init-one))
    (print-rx-program (compile spec-act-no-init-one))
    (print-rx-program (compile spec-no-act-no-init-mul))
    (print-rx-program (compile spec-act-no-init-mul))
    (print-rx-program (compile spec-no-act-init-one))
    (print-rx-program (compile spec-act-init-one))
    (print-rx-program (compile spec-no-act-init-mul))
    (print-rx-program (compile spec-act-init-mul))
    (print-rx-program (compile spec-no-act-seed-one))
    (print-rx-program (compile spec-act-seed-one))
    (print-rx-program (compile spec-no-act-seed-mul))
    (print-rx-program (compile spec-act-seed-mul))

    ))

                
     
(main)