#lang racket/gui
(struct band-user-struct (name surname ID password secret-answer acc-type) #:mutable)
(struct fan-user-struct (name surname ID password secret-answer acc-type) #:mutable)
(struct logged-in-struct (ID acc-type) #:mutable)
;(struct band-listings ()
; Band and fan test account will be deleted
(define test-acc(band-user-struct "Ugur" "Ersoy" 123 123 "test" 0))
(define test-acc2(band-user-struct "Murat" "Ersoy" 345 345 "test2" 0))
(define fan-test-acc (fan-user-struct "Alice" "Johnson" 6969 5252 "test" 1))
(define fan-test-acc2 (fan-user-struct "Bob" "Smith" 3152 3131 "test2" 1))
;(define test-logged-in (logged-in (band-user-struct-ID test-acc) (band-user-struct-acc-type test-acc)))

;;;;;;;;;;;;;;

;band and fan list -> struct type variables will be hold here
(define band-list(list test-acc test-acc2))
(define fan-list(list fan-test-acc fan-test-acc2))
(define logged-in-acc(list))


; creates account by invoking band or fan struct. As list isn't mutable it creates a new list and assigns it to our band or fan-list variable.
; after it is only read-only but you can change what its pointing at (const char list[2]); -> read-only (const char const idf) -> read only + idf cannot be setted


;create functions
(define (create-account name surname id password radiobutton secret-answer)
  (cond
    ((and name surname id password radiobutton)
     (cond
       ((equal? radiobutton 0) (let ((new_account (band-user-struct name surname id password secret-answer 0)))
                                 (set! band-list (cons new_account band-list))))
       ((equal? radiobutton 1) (let ((new_account (fan-user-struct name surname id password secret-answer 1)))
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

(define (set-logged-in-acc st-name)
   (set! logged-in-acc (cons st-name logged-in-acc)))

(define (set-login-empty)
  (set! logged-in-acc '()))

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
(define (get-account-name struct-name account-type)
  (cond
    (
     (equal? account-type 0) (band-user-struct-name struct-name))
    ((equal? account-type 1) (fan-user-struct-name struct-name))
    (else (displayln "Error Secret Answer not Found"))))

(define id-holder 0)
(define name-holder "test_name")
(define (get-logged-acc-dets)
  (let ([flag 0])
  (cond
    ((for
         ([i logged-in-acc])
       (cond
         ((band-user-struct? i)
       (for
           ([k band-list])
         (cond
         ((equal? (band-user-struct-ID i) (band-user-struct-ID k))(set! id-holder (band-user-struct-ID k)) (set! name-holder (band-user-struct-name k)) (set! flag 1)) )))))))
    (cond
      ((and (eq? flag 0)
            (for
                 ([i logged-in-acc])
               (for
                   ([k fan-list])
                 (cond
                 ((equal? (fan-user-struct-ID i) (fan-user-struct-ID k)) (set! name-holder (fan-user-struct-name k)) (set! id-holder (fan-user-struct-ID k)))))))))))
     


(provide (all-defined-out))

