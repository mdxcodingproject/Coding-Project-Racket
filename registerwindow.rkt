#lang racket/gui
(require "structs.rkt")
(define (open-registration-window)
  (define new-window (new frame% [label "Register User"] [width 200] [height 100]))
  (define app-radio-button (new radio-box% [parent new-window] [label ""] [choices (list "Band" "Fan")]))

  ;name - surname - id - password text-fields
  (define user-id-field (new text-field% [parent new-window][enabled #f][label "ID"]))
  (send user-id-field set-value  (number->string (generate-random-uid)))
  (define user-name-field (new text-field% [parent new-window][label "Name"]))
  (define user-surname-field (new text-field% [parent new-window][label "Surname"]))
  (define user-password-field (new text-field% [parent new-window][label "Password"]))
  (define user-secret-answer (new text-field% [parent new-window][label "Secret Answer"]))
  (define (get-user-radio-selection) (send app-radio-button get-selection))
  
  (define submit-button(new button%[parent new-window][label "Submit"]
                            [callback (lambda (button event)
                                        (let* ((id (send user-id-field get-value))
                                               (name (send user-name-field get-value))
                                               (surname (send user-surname-field get-value))
                                               (secret-answer (send user-secret-answer get-value)) 
                                               (password (send user-password-field get-value))
                                               (user-type (send app-radio-button get-selection))) 
                                          (create-account name surname id password user-type secret-answer)
                                          (check-registration-status id user-type)
                                          (cond
                                           ((equal? registration-status 1)
                                            (message-box "Warning"
                                                         (format "Account Has Been Created \nAccount ID: ~a\nAccount Type: ~a" id (send app-radio-button get-item-label (send app-radio-button get-selection)))))
                                           (else "Not registered"))
                                          (send new-window show #f)))]))
  (send new-window show #t))
(provide (all-defined-out))