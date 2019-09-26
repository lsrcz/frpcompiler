#lang rosette/safe

(provide (all-defined-out))

(struct analyzed-value (call unsub processed-num processed-num-non-empty) #:transparent)
(struct analyzed-new-stream (sons processed-num processed-num-non-empty) #:transparent)