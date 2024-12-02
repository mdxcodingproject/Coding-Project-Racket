#lang racket/gui
(require "structs.rkt")
(define fan-screen-status 0)
(define selected-screen-status 0)

(define (set-fan-screen val)
  (set! fan-screen-status val))
(define (set-selected-screen val)
  (set! selected-screen-status val))
;(struct band-listing-struct(concertID bandID bandName date time venue cost seat) #:mutable #:transparent)
;(define listed-concerts(list listing-test listing-test2 listing-test3))
(define fan-band-name-list (list))
(define fan-date-list (list))
(define fan-time-list (list))
(define fan-venue-list (list))
(define fan-cost-list (list))
(define fan-bookStatus-list (list))
(define fan-band-id-list (list))

(define saved-bname-list (list))
(define saved-date-list (list))
(define saved-time-list (list))
(define saved-venue-list (list))
(define saved-cost-list (list))
(define saved-bookStatus-list (list))
(define saved-concert-id-list (list))



(define (clear-list)
  (set! fan-band-name-list (list)) ; GPT -> to clean my list box in order to not copy same concerts over and over again
  (set! fan-date-list (list))
  (set! fan-time-list (list))
  (set! fan-venue-list (list))
  (set! fan-cost-list (list))
  (set! fan-bookStatus-list (list))
  (set! fan-band-id-list (list))) ; -> GPT

(define (set-fan-list st-name)
  (set! fan-band-name-list (cons (band-listing-struct-bandName st-name) fan-band-name-list))
  (set! fan-date-list (cons (band-listing-struct-date st-name) fan-date-list))
  (set! fan-time-list (cons (band-listing-struct-time st-name) fan-time-list))
  (set! fan-venue-list (cons (band-listing-struct-venue st-name) fan-venue-list))
  (set! fan-cost-list (cons (band-listing-struct-cost st-name) fan-cost-list))
  (set! fan-band-id-list (cons (band-listing-struct-concertID st-name) fan-band-id-list))
  (set! fan-bookStatus-list (cons (band-listing-struct-seat st-name) fan-bookStatus-list)))

(define (update-fan-concert-lists list-box)
  (clear-list)
  (for ([i listed-concerts])
    (set-fan-list i))

  (send/apply list-box set (list fan-band-id-list fan-band-name-list fan-date-list fan-time-list fan-venue-list fan-cost-list fan-bookStatus-list))) ; send/apply method is from stackoverflow

;(search-fan-concert-lists saved-listing-lb "ID" saved-concerts-id-list)
(define (search-fan-concert-lists list-box selected text-field)
  (clear-list)
  (cond
    ((equal? selected "")
     (message-box "Warning" "Please select a filter option"))
    ((equal? text-field "")
     (message-box "Warning" "No concert was found")))
  
  (cond
    ((equal? selected "Band Name")
     (for ([i listed-concerts])
       (cond
         ((equal? text-field (band-listing-struct-bandName i))
          (set-fan-list i)
          (send/apply list-box set (list fan-band-id-list fan-band-name-list fan-date-list fan-time-list fan-venue-list fan-cost-list fan-bookStatus-list))))))
    ((equal? selected "Date")
     (for ([i listed-concerts])
       (cond
         ((equal? text-field (band-listing-struct-date i))
          (set-fan-list i)
          (send/apply list-box set (list fan-band-id-list fan-band-name-list fan-date-list fan-time-list fan-venue-list fan-cost-list fan-bookStatus-list))))))
    ((equal? selected "Time")
     (for ([i listed-concerts])
       (cond
         ((equal? text-field (band-listing-struct-time i))
          (set-fan-list i)
          (send/apply list-box set (list fan-band-id-list fan-band-name-list fan-date-list fan-time-list fan-venue-list fan-cost-list fan-bookStatus-list))))))
    ((equal? selected "Location")
     (for ([i listed-concerts])
       (cond
         ((equal? text-field (band-listing-struct-venue i))
          (set-fan-list i)
          (send/apply list-box set (list fan-band-id-list fan-band-name-list fan-date-list fan-time-list fan-venue-list fan-cost-list fan-bookStatus-list))))))
    ((equal? selected "Price")
     (for ([i listed-concerts])
       (cond
         ((equal? text-field (band-listing-struct-cost i))
          (set-fan-list i)
          (send/apply list-box set (list fan-band-id-list fan-band-name-list fan-date-list fan-time-list fan-venue-list fan-cost-list fan-bookStatus-list))))))
    ((equal? selected "Seat Left")
     (for ([i listed-concerts])
       (cond
          ((<= (string->number text-field) (string->number (band-listing-struct-seat i)))
          (set-fan-list i)
          (send/apply list-box set (list fan-band-id-list fan-band-name-list fan-date-list fan-time-list fan-venue-list fan-cost-list fan-bookStatus-list))))))))

(define (clear-saved-list)
  (set! saved-bname-list (list)) ; GPT -> to clean my list box in order to not copy same concerts over and over again
  (set! saved-date-list (list))
  (set! saved-time-list (list))
  (set! saved-venue-list (list))
  (set! saved-cost-list (list))
  (set! saved-bookStatus-list (list))
  (set! saved-concert-id-list (list)))


(define (set-saved-list st-name)
        (set! saved-bname-list (cons (band-listing-struct-bandName st-name) saved-bname-list))
        (set! saved-date-list (cons (band-listing-struct-date st-name) saved-date-list))
        (set! saved-time-list (cons (band-listing-struct-time st-name) saved-time-list))
        (set! saved-venue-list (cons (band-listing-struct-venue st-name) saved-venue-list))
        (set! saved-cost-list (cons (band-listing-struct-cost st-name) saved-cost-list))
        (set! saved-bookStatus-list (cons (band-listing-struct-seat st-name) saved-bookStatus-list))
        (set! saved-concert-id-list (cons (band-listing-struct-concertID st-name) saved-concert-id-list)))


(define (saved-concerts)
  (define (append-to-saved-lb)
      (clear-saved-list)
    (for ([i listed-concerts])
      (cond
        ((list? (member (band-listing-struct-concertID i) fan-selected-concerts))
         (set-saved-list i)
         (send/apply saved-listing-lb set (list saved-concert-id-list saved-bname-list saved-date-list saved-time-list saved-venue-list saved-cost-list saved-bookStatus-list))))))
  
  (define saved-concerts-frame (new frame% [label "Saved Concerts"] [width 900] [height 400]))
  (define hori-top-pane (new horizontal-pane% [parent saved-concerts-frame]))
  (define hori-bot-pane (new horizontal-pane% [parent saved-concerts-frame]))
  (define saved-listing-lb (new list-box% [parent hori-top-pane] [label ""] [min-width 500] [min-height 400]
                                [style (list 'single 'column-headers 'variable-columns)]
                                [columns (list "ID" "Band Name" "Date" "Time" "Location" "Price" "Seat Left")]
                                [choices '()]))
  
  (define remove-id-tf (new text-field% [parent hori-bot-pane] [label "ID"]))
  (define remove-id-button (new button% [parent hori-bot-pane] [label "Remove"]
                                [callback (lambda (button event)
                                            (remove-from-saved (send remove-id-tf get-value))
                                            (append-to-saved-lb))]))
  (append-to-saved-lb)
  (send saved-listing-lb set-column-width 0 100 100 300)
  (send saved-listing-lb set-column-width 1 100 100 300)
  (send saved-listing-lb set-column-width 2 100 100 300)
  (send saved-listing-lb set-column-width 3 180 100 300)
  (send saved-listing-lb set-column-width 4 100 100 300)
  (send saved-listing-lb set-column-width 5 100 100 300)
  (send saved-listing-lb set-column-width 6 100 100 300)
  (send saved-concerts-frame show #t))


(define (show-fan-screen)
  (cond
    ((equal? fan-screen-status 1)
     (define fan-listing-dashboard (new frame% [label "Show Listing"] [width 900] [height 400])) 
     (define hori-top-pane (new horizontal-pane% [parent fan-listing-dashboard]))
     (define fan-listing-lb (new list-box% [parent hori-top-pane] [label ""] [min-width 500] [min-height 400]
                                 [style (list 'single 'column-headers 'variable-columns)]
                                 [columns (list "ID" "Band Name" "Date" "Time" "Location" "Price" "Seat Left")]
                                 [choices '()]))
     
     (define hori-bot-pane (new horizontal-pane% [parent fan-listing-dashboard]))
     (define left-vert-pane (new vertical-pane% [parent hori-bot-pane]))
     
     (define right-vert-pane (new vertical-pane% [parent hori-bot-pane]))
     (define right-vert-hori-pane (new horizontal-pane% [parent right-vert-pane]))

     (define search "")
     (define search-combo-field (new combo-field% [parent left-vert-pane] [label "Filter"] [choices (list "Band Name" "Date" "Time" "Location" "Price" "Seat Left")]
                             [callback
                              (lambda (combo event)
                                (set! search (send search-combo-field get-value)))]))
     (define search-text-field (new text-field% [parent left-vert-pane] [label "Search"]))


     (define search-button (new button% [parent right-vert-hori-pane] [label "Search"] [min-width 100] [min-height 100]
                                [callback (lambda (button event)
                                            (let* ([search-textf (send search-text-field get-value)])
                                              (search-fan-concert-lists fan-listing-lb search search-textf)))]))
     
     (define refresh-button (new button% [parent right-vert-hori-pane] [label "Refresh"] [min-width 100] [min-height 100]
                                 [callback (lambda (button event)
                                             (update-fan-concert-lists fan-listing-lb))]))
     
     (define right-vert-pane-hori-pane (new vertical-pane% [parent right-vert-hori-pane]))
     (define get-concert-id (new text-field% [parent right-vert-pane-hori-pane] [label "Concert ID"]))
     (define save-concert-button (new button% [parent right-vert-pane-hori-pane] [label "Save"] [min-width 200] [min-height 100]
                                      [callback (lambda (button event)
                                                  (set-selected-concerts (send get-concert-id get-value)))]))
     (update-fan-concert-lists fan-listing-lb)
     (send fan-listing-lb set-column-width 0 100 100 300)
     (send fan-listing-lb set-column-width 1 100 100 300)
     (send fan-listing-lb set-column-width 2 100 100 300)
     (send fan-listing-lb set-column-width 3 180 100 300)
     (send fan-listing-lb set-column-width 4 100 100 300)
     (send fan-listing-lb set-column-width 5 100 100 300)
     (send fan-listing-lb set-column-width 6 100 100 300)
     (send fan-listing-dashboard show #t))))









(provide (all-defined-out))