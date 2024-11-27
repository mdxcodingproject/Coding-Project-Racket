#lang racket/gui
(require "structs.rkt")
;(define (register-user user-input-id user-group-selection)
;  (define (register-user-succesful)
;    (message-box "Warning" "Account Created Succesfully"))
;  (define (register-user-failure)
;    (message-box "Warning" "Account Creation Failed!"))
;  (cond
;    ((equal? user-group-selection 0) (append-to-set user-input-id "Band") (register-user-succesful))
;    (else
;     (cond
;       ((equal? user-group-selection 1) (append-to-set user-input-id "Fan") (register-user-failure))
;       (else register-user-failure)))))


(define (open-registration-window)
  (define new-window (new frame% [label "Register User"] [width 200] [height 100]))
  (define app-radio-button (new radio-box% [parent new-window] [label ""] [choices (list "Band" "Fan")]))

  ;name - surname - id - password text-fields
  (define user-id-field (new text-field% [parent new-window][label "ID"]))
  (define user-name-field (new text-field% [parent new-window][label "Name"]))
  (define user-surname-field (new text-field% [parent new-window][label "Surname"]))
  (define user-password-field (new text-field% [parent new-window][label "Password"]))
  (define (get-user-radio-selection) (send app-radio-button get-selection))
  
  (define submit-button(new button%[parent new-window][label "Submit"]
                            [callback (lambda (button event)
                                        (let* ((id (string->number (send user-id-field get-value)))
                                               (name (send user-name-field get-value))
                                               (surname (send user-surname-field get-value))
                                               (password (string->number (send user-password-field get-value)))
                                               (user-type (send app-radio-button get-selection))) ; Bind the user type
                                          (create-account name surname id password user-type) ; Call create-account after bindings
                                          (cond
                                           ((check-registration-status id user-type)(message-box "Warning" (format "Account Has Been Created \nAccount ID: ~a" id)))
                                           (else "Not registered"))
                                          (send new-window show #f)))]))
  (send new-window show #t))
(provide (all-defined-out))