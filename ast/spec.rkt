#lang rosette/safe

(provide (all-defined-out))

(struct spec (inputs output funclist constantlist defaultval body) #:transparent)
