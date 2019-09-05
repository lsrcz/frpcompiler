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
   '(mode down move)
   'drawing
   '(is_undefined is_curve_drawing l n)
   '(empty_list)
   '((mode
      (split ((mode_snapshot mode)
              (down_snapshot down))
             (if-else (is_curve_drawing mode_snapshot)
                      (new-stream
                       ((move (if-else (is_undefined drawing)
                                       (return (l down_snapshot move))
                                       (return (n drawing (prev move) move))))))
                      (new-stream-initial () empty_list)))))))



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
