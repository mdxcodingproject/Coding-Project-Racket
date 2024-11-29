#lang racket/gui
(require "structs.rkt")
(require "editdetails.rkt")
(define band-screen 0)
(define listing-screen 0)


(define band-name-list (list))
(define date-list (list))
(define time-list (list))
(define venue-list (list))
(define cost-list (list))
(define bookStatus-list (list))
(define band-id-list (list))

(define (test-loop-band) ; will be deleted
  (for ([i listed-concerts])
    (printf "~a ~a ~a ~a ~a ~a ~a\n"
            (band-listing-struct-concertID i)
            (band-listing-struct-bandName i)
            (band-listing-struct-date i)
            (band-listing-struct-time i)
            (band-listing-struct-venue i)
            (band-listing-struct-cost i)
            (band-listing-struct-seat i))))
            
;Set functions
;(band-user-struct (list-ref logged-in-acc 0))
(define (update-concert-lists list-box)
  (set! band-name-list (list)) ; gpt told me to clean my list box in order to not copy same concerts over and over again
  (set! date-list (list))
  (set! time-list (list))
  (set! venue-list (list))
  (set! cost-list (list))
  (set! bookStatus-list (list))
  (set! band-id-list (list))
  
  (for ([i listed-concerts])
    (set! band-name-list (cons (band-listing-struct-bandName i) band-name-list))
    (set! date-list (cons (band-listing-struct-date i) date-list))
    (set! time-list (cons (band-listing-struct-time i) time-list))
    (set! venue-list (cons (band-listing-struct-venue i) venue-list))
    (set! cost-list (cons (band-listing-struct-cost i) cost-list))
    (set! band-id-list (cons (band-listing-struct-concertID i) band-id-list))
    (set! bookStatus-list (cons (band-listing-struct-seat i) bookStatus-list)))

  (send/apply list-box set (list band-id-list band-name-list date-list time-list venue-list cost-list bookStatus-list)))


(define (set-band-screen status)
  (set! band-screen status))
(define (set-listing-screen status)
  (set! listing-screen status))

; holds concert(s) information for logged-in band 
(define concert-list-box3 (list band-name-list
                                       date-list
                                       time-list
                                       venue-list
                                       cost-list
                                       band-id-list
                                       bookStatus-list))
         
;main frame
(define (band-show-listings); is being implemented
  (cond
    ((eq? listing-screen 1)
     (define listing-dashboard (new frame% [label "Show Listing"] [width 900] [height 400])) ; thanks to the guy who gave me the idea how to use listbox from stackoverflow
     (define hori-top-pane (new horizontal-pane% [parent listing-dashboard])) ; https://stackoverflow.com/questions/16646910/gui-table-using-in-racket-variable-parameters-using-list-box 
     (define hori-bot-pane (new horizontal-pane% [parent listing-dashboard]))
     (define band-listing-lb (new list-box% [parent hori-top-pane] [label ""] [min-width 500] [min-height 400]
                                  [style (list 'single 'column-headers 'variable-columns)]
                                  [columns (list "ID" "Band Name" "Date" "Time" "Location" "Price" "Seat Left")]
                                  [choices '()]))
                                  ;[callback (lambda (lb event)(displayln "test"))]))
     
     (define edit-dets (new button% [parent hori-bot-pane] [label "Edit Details"] [min-width 200] [min-height 200]
                            [callback (lambda (button event)
                                        (edit-dets-window))]))
     (define refresh-page (new button% [parent hori-bot-pane][label "Refresh"] [min-width 200] [min-height 200] [horiz-margin 300]
                               [callback (lambda (button event)
                                           (update-concert-lists band-listing-lb))]))


     (update-concert-lists band-listing-lb)
     (send band-listing-lb set-column-width 0 100 100 300)
     (send band-listing-lb set-column-width 1 100 100 300)
     (send band-listing-lb set-column-width 2 100 100 300)
     (send band-listing-lb set-column-width 3 180 100 300)
     (send band-listing-lb set-column-width 4 100 100 300)
     (send band-listing-lb set-column-width 5 100 100 300)
     (send band-listing-lb set-column-width 6 100 100 300)
     (send listing-dashboard show #t))))
        
(define (band-create-listings) ; need date structure on top right -> install gregor module
  (cond
    ((eq? band-screen 1)
     (define create-listings-frame (new frame% [label "Create Listings"] [width 600] [height 400]))
     (define listings-hor-top (new horizontal-pane% [parent create-listings-frame]))
     (define listings-hor-bot (new horizontal-pane% [parent create-listings-frame]))
     
     (define listings-ver-top-left (new vertical-pane% [parent listings-hor-top]))
     (define listings-ver-top-right (new vertical-pane% [parent listings-hor-top]))
     
     (define listings-ver-bot-left (new vertical-pane% [parent listings-hor-bot]))
     (define listings-ver-bot-right (new vertical-pane% [parent listings-hor-bot]))
     
     (define top-left (new group-box-panel% [parent listings-ver-top-left] [label ""] [stretchable-width #f] [stretchable-height #f]))
     (define band-id-message(new message% (parent top-left)[enabled #f] (label (format "Band ID: ~a Band Name: ~a" id-holder name-holder)) [min-width 200] [min-height 0] [font (make-object font%)]))
     
     (define top-right (new group-box-panel% [parent listings-ver-top-right] [label "top-right"]))


     
     (define bot-right (new group-box-panel% [parent listings-ver-bot-right] [label ""]))
     
     (define bot-left (new group-box-panel% [parent listings-ver-bot-left] [label ""] [font (make-object font%)]))
     (define name-text-field (new text-field% [parent bot-left] [label "Band N."] [enabled #f]))
     (define date-hor-pane (new horizontal-pane% [parent bot-left] [stretchable-height #f])) ; this is for date, so it can have 3 text-fields next to each other for DD, MM AND YYYY
     (define dated-text-field (new text-field% [parent date-hor-pane] [label "Date(DD/MM/YYYY)"]))
     (define datem-text-field (new text-field% [parent date-hor-pane] [label ""]))
     (define datey-text-field (new text-field% [parent date-hor-pane] [label ""]))
     (define time-text-field (new text-field% [parent bot-left] [label "Time"]))
     (define loc-text-field (new text-field% [parent bot-left] [label "Location"]))
     (define price-text-field (new text-field% [parent bot-left] [label "Price"]))
     (define booking-status (new text-field% [parent bot-left] [label "Seat"]))

     (define create-listing-button (new button% [parent bot-right] [label "Create Concert Listing"]
                                        [enabled #t] [min-width 200] [min-height 200]
                                        [callback (lambda (button event)
                                                    (let* ([name (send name-text-field get-value)]
                                                           (concertID 0)
                                                           (dated (send dated-text-field get-value))
                                                           (datem (send datem-text-field get-value))
                                                           (datey (send datey-text-field get-value))
                                                           (time (send time-text-field get-value))
                                                           (date-string (string-append dated"/"datem"/"datey))
                                                           (location (send loc-text-field get-value))
                                                           (seat (send booking-status get-value))
                                                           (price (send price-text-field get-value)))
                                                      (create-concert-listing concertID id-holder name date-string time location price seat)
                                                      (test-loop-band)
                                                      ))]))
     (send dated-text-field set-value "13")
     (send datem-text-field set-value "05")
     (send datey-text-field set-value "2025")
     (send time-text-field set-value "13:55")
     (send booking-status set-value "100")
     (send loc-text-field set-value "London")
     (send price-text-field set-value "24.99$")
     ;(define (create-concert-listing bandid name date time venue cost bookStatus)
     
     (send name-text-field set-value name-holder)
     (send create-listings-frame show #t))))




(provide (all-defined-out))
