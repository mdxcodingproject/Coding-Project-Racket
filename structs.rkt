#lang racket/gui
(struct band-user-struct (name surname ID password) #:mutable)
(struct fan-user-struct (name surname ID password) #:mutable)
(define band-list(list))
(define fan-list(list))

(define (create-account name surname id password radiobutton)
  (cond
    ((and name surname id password radiobutton)
     (cond
       ((equal? radiobutton 0) (let ((new_account (band-user-struct name surname id password)))
                              (set! band-list (cons new_account band-list))))
       ((equal? radiobutton 1) (let ((new_account (fan-user-struct name surname id password)))
                              (set! band-list (cons new_account fan-list))))))
    (else (message-box "Warning" "Invalid Argument(s)! All text fields must be filled!\n"))))



(define (check-registration-status id radio-selection)
  (cond
    ((equal? radio-selection 0)
     (for ([i band-list])
       (equal? id (band-user-struct-ID i)) #t))
    ((equal? radio-selection 1)
     (for ([i fan-list])
       (equal? id (fan-user-struct-ID i)) #t))
    (else #f)))

(provide (all-defined-out))

