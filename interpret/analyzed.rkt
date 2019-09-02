#lang racket

(provide (all-defined-out))

(struct analyzed-value (call unsub) #:transparent)
