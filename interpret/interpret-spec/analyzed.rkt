#lang rosette/safe

(provide (all-defined-out))

(struct analyzed-value (call sons processed-num processed-num-non-empty) #:transparent)
(struct analyzed-new-stream (sons processed-num processed-num-non-empty) #:transparent)