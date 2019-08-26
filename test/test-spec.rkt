#lang racket

(provide (all-defined-out))

(struct spec (inputs output funclist constantlist body) #:transparent)

(define drawing-spec
  (spec
   '(mode move down)
   'drawing
   '(f h g m l n)
   '()
   (list
    '(mode
      (if-else (f mode)
          (return (h))
          (return (g))))
    '(move
      (if (f mode)
          (if-else (m drawing)
              (return (l down move))
              (return (n drawing (prev move) move))))))))

(define drawing-custom-spec
  (spec
   '(mode move down)
   'drawing
   '(f h g m l n)
   '()
   (list
    '(mode
      (if-else (f mode)
          (return (h))
          (return (g))))
    '(move
      (if (f mode)
          (custom c1 (if-else (m drawing)
              (return (l down move))
              (return (n drawing (prev move) move)))))))))

(define drawing-split-spec
  (spec
   '(mode down mode)
   'drawing
   '(f g)
   '()
   '((mode
      (split ((mode-snapshot mode)
              (down-snapshot down))
             (if-else (f mode-snapshot)
                      (new-stream
                       ((move (if-else (g drawing)
                                       (return (l down-snapshot move))
                                       (return (n drawing (prev move) move))))))
                      (empty-stream)))))))



(define drawing-modified-spec
  (spec
   '(mode move down)
   'drawing
   '(f h g m l n)
   '()
   (list
    '(mode
      (if-else (f mode)
          (return (h))
          (return (g))))
    '(move
      (if (f mode)
          (if-else (m drawing)
              (return (l down move))
              (return (prev move))))))))
