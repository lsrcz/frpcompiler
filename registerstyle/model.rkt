#lang rosette

(provide (all-defined-out))

;; convenience methods for getting symbolic vars

(define (get-sym-bool)
  (define-symbolic* b boolean?)
  b)
(define (get-sym-int)
  (define-symbolic* i integer?)
  i)

;; In the discrete time model, a stream is represented by a list.
;; Each element in the list represents a tick of the clock.
;; At each timestep, either an event happens, or an event does not happen.
;; If an event occurs at some timestep t, the list stores a value corresponding to the event
;; at the t position of the list.
;; If no event occurs at time t, then a special symbol is stored at the t position of the list

(define NOEVENT 'no-evt)

(define (empty-event-reg? e)
  (eq? NOEVENT e))

(define (not-empty-event-reg? e)
  (not (eq? NOEVENT e)))

;; In a symbolic stream, each element is a symbolic union of the non-event symbol
;; and a symbolic event value

;; example of a single timestep
;; (define-symbolic* event-bool boolean?)
;; (define-symbolic* event-value integer?)
;; (if event-bool event-value NOEVENT)

;; We construct a symbolic stream of bounded length by giving the desired (concrete) length
;; and a constructor for the event value type

(define (new-event-stream constructor n)
  (for/list ([i n])
    (if (get-sym-bool)
        (constructor)
        NOEVENT)))

;; example of a stream of length 3, with integer event values
;; (define int-stream-3 (new-event-stream get-sym-int 3))
