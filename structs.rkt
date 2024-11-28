#lang racket/gui
(struct band-user-struct (name surname ID password secret-answer) #:mutable)
(struct fan-user-struct (name surname ID password secret-answer) #:mutable)


; Band and fan test account will be deleted
(define test-acc(band-user-struct "Ugur" "Ersoy" 123 123 "test"))
(define test-acc2(band-user-struct "Murat" "Ersoy" 345 567 "test2"))
(define fan-test-acc (fan-user-struct "Alice" "Johnson" 6969 5252 "test"))
(define fan-test-acc2 (fan-user-struct "Bob" "Smith" 3152 3131 "test2"))
;;;;;;;;;;;;;;

;band and fan list -> struct type variables will be hold here
(define band-list(list test-acc test-acc2))
(define fan-list(list fan-test-acc fan-test-acc2))


; creates account by invoking band or fan struct. As list isn't mutable it creates a new list and assigns it to our band or fan-list variable.
; after it is only read-only but you can change what its pointing at (const char list[2]); -> read-only (const char const idf) -> read only + idf cannot be setted
(define (create-account name surname id password radiobutton secret-answer)
  (cond
    ((and name surname id password radiobutton)
     (cond
       ((equal? radiobutton 0) (let ((new_account (band-user-struct name surname id password secret-answer)))
                                 (set! band-list (cons new_account band-list))))
       ((equal? radiobutton 1) (let ((new_account (fan-user-struct name surname id password secret-answer)))
                                 (set! fan-list (cons new_account fan-list))))))
    (else (message-box "Warning" "Invalid Argument(s)! All text fields must be filled!\n"))))


(define registration-status 0)

(define (check-registration-status id radio-selection)
  (printf "~a ~a\n" id radio-selection)
  (set! registration-status 0)
  (cond
    ((and (equal? radio-selection 0) (number? id))
     (for ([i band-list])
       (equal? id (band-user-struct-ID i)) (set! registration-status 1) (printf "reg stat: ~a\n" registration-status)))
    ((and(equal? radio-selection 1) (number? id))
     (for ([i fan-list])
       (equal? id (fan-user-struct-ID i)) (set! registration-status 1)))
    (else (set! registration-status 1) (displayln "test\n"))))


;set functions
(define (set-account-password userid new-pass acc-type)
  (let ([old-password 0])
    (cond
      ((and (equal? acc-type 0) (number? userid))
       (for ([i band-list])
         (cond
           ((equal? userid (band-user-struct-ID i)) (set! old-password (band-user-struct-password i)) (set-band-user-struct-password! i (string->number new-pass))
                                                    (message-box "Warning" (format "Old Password: ~a\nNew Paasword: ~a\n Band Name: ~a\nBand ID: ~a/~a" old-password (band-user-struct-password i) (band-user-struct-name i) (band-user-struct-ID i) userid)))))))
    (cond
      ((and (equal? acc-type 1) (number? userid))
       (for ([i fan-list])
         (cond
           ((equal? userid (fan-user-struct-ID i)) (set! old-password (fan-user-struct-password i)) (set-fan-user-struct-password! i new-pass)
                                                   (message-box "Warning" (format "Old Password: ~a\nNew Paasword: ~a\n Fan Name: ~a\nFan ID: ~a/~a" old-password (fan-user-struct-password i) (fan-user-struct-name i) (fan-user-struct-ID i) userid))))))
      (else
       (cond
         ((not (number? userid)) (message-box "Warning" (printf "ID must be a number!"))))))))


;get functions
(define (get-account-password struct-name account-type)
  (cond
    ((equal? account-type 0) (band-user-struct-password struct-name))
    ((equal? account-type 1) (fan-user-struct-password struct-name))
    (else (displayln "Error Pass"))))

(define (get-account-id struct-name account-type)
  (cond
    ((equal? account-type 0) (band-user-struct-ID struct-name))
    ((equal? account-type 1) (fan-user-struct-ID struct-name))
    (else (displayln "Error id"))))

(define (get-account-secret-answer struct-name account-type)
  (cond
    ((equal? account-type 0) (band-user-struct-secret-answer struct-name))
    ((equal? account-type 1) (fan-user-struct-secret-answer struct-name))
    (else (displayln "Error Secret Answer not Found"))))

(provide (all-defined-out))

