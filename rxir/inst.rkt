#lang racket

(provide (all-defined-out))

(struct rx-name-ref (name) #:transparent)
(struct rx-stream-ref (stream) #:transparent)
(struct rx-stream-pair-ref (stream num) #:transparent)
(struct rx-merge (stream-list) #:transparent)
(struct rx-merge-action (stream-list) #:transparent)
(struct rx-pipe (stream ops) #:transparent)
(struct rx-custom (stream name) #:transparent)
(struct rx-empty () #:transparent)
(struct rx-action (action-source) #:transparent)

(struct rx-start-with-undefined () #:transparent)
(struct rx-buffer-count (buffer-size start-buffer-every) #:transparent)
(struct rx-map-shape (from-shape to-shape) #:transparent)
(struct rx-with-latest-from (streams) #:transparent)
(struct rx-map-flat (from-nested-shape to-shape) #:transparent)
(struct rx-filter (from-shape arg) #:transparent)
(struct rx-partition (from-shape arg) #:transparent)
(struct rx-ret-action (from-shape return-val action) #:transparent)
(struct rx-switch-map (from-shape body) #:transparent)

(struct rxir-input-inst (name num inst) #:transparent)
(struct rxir-list (input-insts lst) #:transparent)
