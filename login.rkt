#lang racket/gui
(require "structs.rkt")
(define login-status 0)
;(struct band-user-struct (name surname ID password) #:mutable)
;(struct fan-user-struct (name surname ID password) #:mutable)
(define (test-loop)
  (for ([i band-list])
    (printf "~a\n" (get-account-password i 0))))

(define (authenticate-user user-id user-pass account-type)
  (let ([flag 0])
    (cond
      ((equal? account-type 0)
       (for ([i band-list])
         (cond
           ((and (equal? (get-account-id i 0) user-id) (equal? (get-account-password i 0) user-pass)) (set! flag 1)(message-box "Information" "Band Login Successful") (set! login-status 1))))))
    (cond
      ((equal? account-type 1)
       (for ([i fan-list])
         (cond
           ((and (equal? (get-account-id i 1) user-id) (equal? (get-account-password i 1) user-pass)) (set! flag 1) (message-box "Information" "Fan Login Successful") (set! login-status 1)))))
      (else (set! login-status 0)))
    (cond
      ((equal? flag 0) (message-box "Warning" "Password/ID is wrong\n")))))


(provide (all-defined-out))