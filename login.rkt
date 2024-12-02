#lang racket/gui
(require "structs.rkt")
(define login-status 0)
;test loop will be deleted
(define (test-loop)
  (for ([i fan-list])
    (printf "login>pass:~a id:~a\n" (get-account-password i 1) (get-account-id i 1))))
(define (test)
  (displayln set-logged-in-acc))

(define (authenticate-user user-id user-pass account-type)
  (test-loop)
  (set-login-empty) ; empties the logged-in account list. -> this is being used by band-panel to retrieve band name and ID
  (get-logged-acc-dets)
  (set! login-status 0) ; if band/fan exits from their account and try to login in a different account this is to avoid a bug. 
  (let ([flag 0])
    (cond
      ((equal? account-type 0)
       (for ([i band-list])
         (cond
           ((and (equal? (get-account-id i 0) user-id) (equal? (get-account-password i 0) user-pass)) (set! flag 1)(message-box "Information" "Band Login Successful") (set! login-status 1) (set-logged-in-acc i)
                                                                                                      (get-logged-acc-dets))))))
    (cond
      ((equal? account-type 1)
       (for ([i fan-list])
         (cond
           ((and (equal? (get-account-id i 1) user-id) (equal? (get-account-password i 1) user-pass)) (set! flag 1) (message-box "Information" "Fan Login Successful") (set! login-status 1) (set-logged-in-acc i))))))
    (cond
      ((equal? flag 0) (message-box "Warning" "Password/ID is wrong\n")))))
(provide (all-defined-out))