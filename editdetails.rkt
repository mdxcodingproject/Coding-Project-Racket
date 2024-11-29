#lang racket/gui
(require "structs.rkt")
(define (edit-time-frame)
  (define time-frame (new frame% [label "Edit Time"] [width 300][height 300]))
  (define concert-id-tf (new text-field% [parent time-frame] [label "Concert ID"]))
  (define new-date (new text-field% [parent time-frame] [label "New Date"]))
  (define new-time (new text-field% [parent time-frame] [label "New Time"]))
  (define submit-button (new button% [parent time-frame] [label "Submit"]
                             [callback (lambda (button event)
                                         (let ([uid (send concert-id-tf get-value)]
                                               (date (send new-date get-value))
                                               (time (send new-time get-value)))
                                           (set-date-time uid date time)
                                           ))]))
  (send time-frame show #t))

(define (edit-dets-window)
  (define edit-frame (new frame% [label "Edit Screen"] [width 250] [height 250]))

  (define hori-top-pane (new horizontal-pane% [parent edit-frame]))
  (define hori-bot-pane (new horizontal-pane% [parent edit-frame]))
  (define edit-date-button (new button% [parent hori-top-pane] [label "Time/Date"][min-width 150] [min-height 150]
                                [callback (lambda (button event)
                                            (edit-time-frame))]))
  (define edit-price-button (new button% [parent hori-top-pane] [label "Price"][min-width 150] [min-height 150]))
  (define edit-seat-button (new button% [parent hori-bot-pane] [label "Seat"][min-width 150] [min-height 150]))
  (define edit-cancel-button (new button% [parent hori-bot-pane] [label "Cancelled"][min-width 150] [min-height 150]))





  
  (send edit-frame show #t))

(provide (all-defined-out))
