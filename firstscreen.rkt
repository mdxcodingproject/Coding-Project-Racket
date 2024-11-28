#lang racket/gui
(require "registerwindow.rkt")
(require "login.rkt")
(require "clear-screen.rkt")
(require "passwordreset.rkt")
(require "band-login-screen.rkt")
(require "structs.rkt")
(define is-screen-on 0)
(define window-width 800)
(define window-height 400)


; (define-values (screen_width screen_height) (get-display-size)) ; get screenvalues and print -> for debug delete later on
; (printf "Screen resolution: ~a x ~a\n" screen_width screen_height)

(define (first-screen)
  (define main-window-screen-frame (new frame% [label "Concert App"] [width window-width] [height window-height]))
  (define horizontal-pane-top (new horizontal-pane% [parent main-window-screen-frame]))
  (define horizontal-pane-bottom (new horizontal-pane% [parent main-window-screen-frame]))

  ;pane and group-box for fan and band
  (define vertical-pane-band (new vertical-pane% [parent horizontal-pane-bottom]))
  (define band-group-box (new group-box-panel% [parent vertical-pane-band] [label ""]))
  (define vertical-pane-fan (new vertical-pane% [parent horizontal-pane-bottom]))
  (define fan-group-box (new group-box-panel% [parent vertical-pane-fan] [label ""]))


  ;Buttons and text fields 
  (define registration-button-main-window (new button% [label "Registration"][parent horizontal-pane-top][stretchable-width #t][min-height 100] [min-width 100][horiz-margin (/ window-width 4)]
                                               [callback (lambda (button event) (open-registration-window))]))
  (define signin-button-main-window (new button% [label "Password Reset"][parent horizontal-pane-top][stretchable-width #t][min-height 100][min-width 100][horiz-margin (/ window-width 4)]
                                         [callback (lambda (button event) (password-reset-screen))]))
  (define band-id-text-field (new text-field% [label ""] [parent band-group-box]))
  (define band-password-text-field (new text-field% [label ""] [parent band-group-box]))
  
  (define fan-id-text-field (new text-field% [label ""] [parent fan-group-box]))
  (define fan-password-text-field (new text-field% [label ""] [parent fan-group-box]))
  (define band-login-button(new button% [parent band-group-box] [label "Band Login"] [callback (lambda (button event)
                                                                                                 (authenticate-user (string->number (send band-id-text-field get-value))
                                                                                                                    (string->number (send band-password-text-field get-value)) 0)
                                                                                                 (cond
                                                                                                   ((equal? login-status 1)
                                                                                                    (send main-window-screen-frame show #f) (band-menu))))]))
  (define fan-login-button(new button% [parent fan-group-box] [label "Fan Login"] [callback (lambda (button event)
                                                                                              (authenticate-user (string->number (send fan-id-text-field get-value))
                                                                                                                 (string->number (send fan-password-text-field get-value)) 1)
                                                                                              (cond
                                                                                                ((equal? login-status 1)
                                                                                                 (send main-window-screen-frame show #f))))])) ; method will be changed -> destroy firstscreen and open new window
  (send band-id-text-field set-value "123")
  (send band-password-text-field set-value "123")
  (send fan-id-text-field set-value "Fan ID")
  (send fan-password-text-field set-value "Fan Password")
  (send main-window-screen-frame show #t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define band-screen 0)

(define (band-menu)
  (cond
    ((eq? login-status 1)
     (define band-dashboard (new frame% [label "Dashboard"][width 400] [height 400]))
     (define dashboard-top-pane-horizontal (new horizontal-pane% [parent band-dashboard]))
     (define dashboard-bot-pane-horizontal (new horizontal-pane% [parent band-dashboard]))
  
     (define test (new button% [label "2.1"] [parent dashboard-top-pane-horizontal][stretchable-width #t][min-height 100] [min-width 100][horiz-margin (/ window-width 4)]
                       [callback (lambda (button event)
                                   (set! band-screen 1) (band-create-listings))]))
     (define test2 (new button% [label "2.2"] [parent dashboard-top-pane-horizontal][stretchable-width #t][min-height 100] [min-width 100][horiz-margin (/ window-width 4)]))

     (define go-back-button (new button% [label "GO BACK\n<<<<<<"] [parent dashboard-bot-pane-horizontal] [stretchable-width #f][min-height 100] [min-width 100]
                                 [callback (lambda (button event)
                                             (send band-dashboard show #f) (first-screen))]))

  
     ;(define db-left (new vertical-pane% [parent band-create-listing-dashboard]))
        
        
     (send band-dashboard show #t))))
(define (band-create-listings)
  (cond
    ((eq? band-screen 1)
     (define create-listings-frame (new frame% [label "Dashboard"] [width 600] [height 400]))
     (define listings-hor-top (new horizontal-pane% [parent create-listings-frame]))
     (define listings-hor-bot (new horizontal-pane% [parent create-listings-frame]))
     
     (define listings-ver-top-left (new vertical-pane% [parent listings-hor-top]))
     (define listings-ver-top-right (new vertical-pane% [parent listings-hor-top]))
     
     (define listings-ver-bot-left (new vertical-pane% [parent listings-hor-bot]))
     (define listings-ver-bot-right (new vertical-pane% [parent listings-hor-bot]))
     
     (define top-left (new group-box-panel% [parent listings-ver-top-left] [label "top-left"] [stretchable-width #f] [stretchable-height #f]))
     (define band-id-message(new message% (parent top-left)[enabled #f] (label (format "Band ID: ~a Band Name: ~a"(number->string id-holder) name-holder)) [min-width 200] [min-height 0] [font (make-object font%)]))
     
     (define top-right (new group-box-panel% [parent listings-ver-top-right] [label "top-right"]))


     
     (define bot-right (new group-box-panel% [parent listings-ver-bot-right] [label "bot-right"]))
     (define create-listing-button (new button% [parent bot-right] [label "Create Concert Listing"]
                                        [enabled #t] [min-width 200] [min-height 200]
                                        [callback (lambda (button event)
                                                    (displayln "not yet"))]))
     (define bot-left (new group-box-panel% [parent listings-ver-bot-left] [label "bot-left"] [font (make-object font%)]))
     (define name-text-field (new text-field% [parent bot-left] [label "Band N."] [enabled #f]))
     (define date-text-field (new text-field% [parent bot-left] [label "Date"]))
     (define loc-text-field (new text-field% [parent bot-left] [label "Location"]))
     (define price-text-field (new text-field% [parent bot-left] [label "Price"]))
     
     
     (send name-text-field set-value name-holder)
     (send create-listings-frame show #t))))





(band-create-listings) 
(first-screen)
(band-menu)