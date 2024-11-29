#lang racket/gui
(require "structs.rkt")
(define uid 0)

(define (edit-seat)
  (define seat-frame (new frame% [label "Edit Seat"] [width 300]))
  (define seat-status (new text-field% [parent seat-frame] [label "Seat Left"]))
  (define submit-button (new button% [parent seat-frame] [label "Submit"]
                             [callback (lambda (button event)
                                         (set-seat uid (send seat-status get-value)) (send seat-frame show #f))]))
  (send seat-frame show #t))
(define (edit-price)
  (define price-frame (new frame% [label "Edit Price"] [width 300]))
  (define new-price-field (new text-field% [parent price-frame] [label "New Price"]))
  (define submit-button (new button% [parent price-frame] [label "Submit"]
                             [callback (lambda (button event)
                                         (let* ([new-price (string-append (send new-price-field get-value)"£")])
                                           (set-new-price uid new-price)(send price-frame show #f)))]))
  
  (send price-frame show #t))

(define (edit-time-frame)
  (define time-frame (new frame% [label "Edit Time"] [width 300]))
  (define hori-top-pane (new horizontal-pane% [parent time-frame]))
  (define new-dated (new text-field% [parent hori-top-pane] [label "New Date(DD/MM/YYYY)"]))
  (define new-datem (new text-field% [parent hori-top-pane] [label "/"]))
  (define new-datey (new text-field% [parent hori-top-pane] [label "/"]))
  (define hori-bot-pane (new horizontal-pane% [parent time-frame]))
  (define new-timeh (new text-field% [parent hori-bot-pane] [label "New Time(HH:MM)"]))
  (define new-timem (new text-field% [parent hori-bot-pane] [label ":"]))
  (define submit-button (new button% [parent time-frame] [label "Submit"]
                             [callback (lambda (button event)
                                         (let* (
                                                (dated (send new-dated get-value))
                                                (datem (send new-datem get-value))
                                                (datey (send new-datey get-value))
                                                (date-string (string-append dated"/"datem"/"datey))
                                                (timeh (send new-timeh get-value))
                                                (timey (send new-timem get-value))
                                                (time-string (string-append timeh":"timey)))
                                           
                                           (set-date-time uid date-string time-string)
                                           ))]))
  (send time-frame show #t))

(define (edit-dets-window)
  (define edit-frame (new frame% [label "Edit Screen"] [width 250] [height 250]))

  (define hori-top-pane (new horizontal-pane% [parent edit-frame]))
  (define hori-bot-pane (new horizontal-pane% [parent edit-frame]))
  (define concert-uid-pane (new horizontal-pane% [parent edit-frame]))
  (define edit-date-button (new button% [parent hori-top-pane] [label "Time/Date"][stretchable-width #t][stretchable-height #t][min-width 150] [min-height 150]
                                [callback (lambda (button event)
                                            (edit-time-frame))]))
  (define edit-price-button (new button% [parent hori-top-pane] [label "Price"][stretchable-width #t][stretchable-height #t][min-width 150] [min-height 150]
                                 [callback (lambda (button event)
                                             (edit-price))]))
  (define edit-seat-button (new button% [parent hori-bot-pane] [label "Seat"][stretchable-width #t][stretchable-height #t][min-width 150] [min-height 150]
                                [callback (lambda (button event)
                                            (edit-seat))]))
  (define edit-cancel-button (new button% [parent hori-bot-pane] [label "Cancelled"][stretchable-width #t][stretchable-height #t][min-width 150] [min-height 150]
                                  [callback (lambda (button event)
                                              (cancel-concert uid))]))
  (define concert-uid (new text-field% [parent concert-uid-pane] [label "Concert-UID"][stretchable-height #t]
                           [callback (lambda (tf event)
                                       (set! uid (send concert-uid get-value)))]))
  
  (send edit-frame show #t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide (all-defined-out))
