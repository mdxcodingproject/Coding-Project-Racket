#lang racket/gui
(define (clear-frame frame)
  (for ([child (send frame get-children)])
    (send child show #f)))
(provide (all-defined-out))