#lang rosette
(provide (all-defined-out))

(require "model.rkt")

;; For our interpreter, we write Rosette implementations of all supported
;; RxJS operators that we want to use for synthesis

;; Each operator takes one or more streams as arguments and returns a stream

;; Note that some operators have the same names as Racket methods,
;; so we always add the prefix rx

(define (getStatefulStream f init stream)
  (for/list ([i (range 1 (add1 (length stream)))])
    (foldl f init (take stream i))))

;; http://reactivex.io/documentation/operators/combinelatest.html
(define (rxCombineLatest f inputStream1 inputStream2)
  (let ([stateStream1 (getStatefulStream (λ (evt accum)
                                 (if (empty-event? evt)
                                     accum
                                     evt)) NOEVENT inputStream1)]
        [stateStream2 (getStatefulStream (λ (evt accum)
                                 (if (empty-event? evt)
                                     accum
                                     evt)) NOEVENT inputStream2)])
    (for/list ([input-event1 inputStream1]
               [input-event2 inputStream2]
               [state-event1 stateStream1]
               [state-event2 stateStream2])
      (if (and (or (not-empty-event? input-event1) (not-empty-event? input-event2))
               (not-empty-event? state-event1) (not-empty-event? state-event2))
          (f state-event1 state-event2)
          NOEVENT))))

;; http://reactivex.io/documentation/operators/distinct.html
(define (rxDistinct inputStream)
  (let ([stateStream (getStatefulStream (λ (evt accum)
                                 (if (empty-event? evt)
                                     accum
                                     evt)) NOEVENT inputStream)])
    (cons (first inputStream)
          (for/list ([input-event (cdr inputStream)]
                     [previous-state stateStream])
            (if (not (equal? input-event previous-state))
                input-event
                NOEVENT)))))

;; http://reactivex.io/documentation/operators/filter.html
(define (rxFilter pred inputStream)
  (map (λ (evt) (if (or (empty-event? evt) (not (pred evt)))
                    NOEVENT
                    evt)) inputStream))

;; http://reactivex.io/documentation/operators/map.html
(define (rxMap f inputStream)
  (for/list ([elt inputStream])
    (if (not-empty-event? elt)
        (f elt)
        NOEVENT)))

;; http://reactivex.io/documentation/operators/merge.html
(define (rxMerge stream1 stream2)
  (for/list ([elt1 stream1]
             [elt2 stream2])
    (if (not-empty-event? elt1) elt1 elt2)))

;; http://reactivex.io/documentation/operators/buffer.html
(define (rxPairwise inputStream)
  (for/list ([i (range (length inputStream))])
    (if (or (empty-event? (list-ref inputStream i))
            (andmap empty-event? (take inputStream i)))
        NOEVENT
        (cons (foldl (λ (evt accum)
                       (if (and (empty-event? accum) (not-empty-event? evt))
                           evt
                           accum)) NOEVENT (reverse (take inputStream i))) (list-ref inputStream i)))))

;; http://reactivex.io/documentation/operators/scan.html
(define (rxScan f inputStream)
  (let ([accumStream (getStatefulStream (λ (evt accum)
                                (if (empty-event? evt)
                                    accum
                                    (if (empty-event? accum)
                                        evt
                                        (f accum evt)))) NOEVENT inputStream)])
    (for/list ([input-event inputStream]
               [accum-event accumStream])
      (if (empty-event? input-event)
          NOEVENT
          accum-event))))

;; http://reactivex.io/documentation/operators/skip.html
(define (rxSkip n inputStream)
  (let ([eventCounter (getStatefulStream (λ (evt accum)
                                 (if (empty-event? evt)
                                     accum
                                     (add1 accum))) 0 inputStream)])
    (for/list ([input-event inputStream]
               [counter-event eventCounter])
      (if (<= counter-event n)
          NOEVENT
          input-event))))


;; http://reactivex.io/documentation/operators/take.html
(define (rxTake n inputStream)
  (let ([eventCounter (getStatefulStream (λ (evt accum)
                                 (if (empty-event? evt)
                                     accum
                                     (add1 accum))) 0 inputStream)])
    (for/list ([input-event inputStream]
               [counter-event eventCounter])
      (if (> counter-event n)
          NOEVENT
          input-event))))