#lang racket/gui
(require "registerwindow.rkt")
(require "structs.rkt")
(define password-reset-flag 0)

(define (find-secret-answer user-id user-secret acc-type)
  (cond
    ((equal? acc-type 0)
     (for ([i band-list])
       (cond
         ((and (equal? user-secret (band-user-struct-secret-answer i)) (equal? user-id (band-user-struct-ID i)))(set! password-reset-flag 1))))))
  (cond
    ((equal? acc-type 1)
     (for ([i fan-list])
       (cond
         ((and (equal? user-secret (fan-user-struct-secret-answer i)) (equal? user-id (fan-user-struct-ID i))) (set! password-reset-flag 1))))))
  (cond
    ((equal? password-reset-flag 0) (message-box "Warning" "no secret answer\n"))))


(define (password-reset-screen)
  (define password-reset-frame (new frame% [label "Password Reset"] [width 300] [height 200]))
  (define passreset-radio-button (new radio-box% [parent password-reset-frame] [label ""] [choices (list "Band" "Fan")]))
  (define user-id-preset-text-field (new text-field% [parent password-reset-frame] [label "User ID"]))
  (define secret-answer-text-field (new text-field% [parent password-reset-frame] [label "Secret Answer"]))
  (define new-password-text-field (new text-field% [parent password-reset-frame] [label "New Password"]))
  (define reset-button (new button% [parent password-reset-frame] [label "Submit Request"] [callback (lambda (button event)
                                                                                                       (let ([new-pass (send new-password-text-field get-value)]
                                                                                                             [user-secret-ans (send secret-answer-text-field get-value)]
                                                                                                             (acc-type (send passreset-radio-button get-selection))
                                                                                                             (user-id (string->number(send user-id-preset-text-field get-value))))
                                                                                                         (printf "Reset Button> S.Ans: ~a Acc-type: ~a\n" user-secret-ans acc-type)
                                                                                                         (find-secret-answer user-id user-secret-ans acc-type)
                                                                                                         (cond
                                                                                                           ((eq? password-reset-flag 1)
                                                                                                            (set-account-password user-id new-pass acc-type)
                                                                                                            (displayln (band))
                                                                                                            (printf "Reset Button> New pass: ~a Acc-type: ~a" new-pass acc-type))
                                                                                                           (else (message-box "Warning" (format "ID: ~a or Secret Answer: ~a wrong!" user-id user-secret-ans))))))]))
                                                                                                          


;  (send user-id-preset-text-field set-value "123")
;  (send secret-answer-text-field set-value "test")
;  (send new-password-text-field set-value "706")

  
  (send password-reset-frame show #t))
(define (band)
  band-list)
(provide (all-defined-out))
;