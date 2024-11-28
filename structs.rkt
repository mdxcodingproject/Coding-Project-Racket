#lang racket/gui
(struct band-user-struct (name surname ID password secret-answer) #:mutable)
(struct fan-user-struct (name surname ID password secret-answer) #:mutable)

(define test-acc(band-user-struct "Ugur" "Ersoy" 123 123 "oyku"))
(define test-acc2(band-user-struct "Murat" "Ersoy" 345 567 "firuze"))


;(printf "SA: ~a\n"(band-user-struct-secret-answer test-acc))


(define fan-test-acc (fan-user-struct "Alice" "Johnson" 6969 5252 "ugur"))
(define fan-test-acc2 (fan-user-struct "Bob" "Smith" 3152 3131 "ersoy"))

(define band-list(list test-acc test-acc2))
(define fan-list(list fan-test-acc fan-test-acc2))

(define (create-account name surname id password radiobutton secret-answer)
  (cond
    ((and name surname id password radiobutton)
     (cond
       ((equal? radiobutton 0) (let ((new_account (band-user-struct name surname id password secret-answer)))
                              (set! band-list (cons new_account band-list))))
       ((equal? radiobutton 1) (let ((new_account (fan-user-struct name surname id password secret-answer)))
                              (set! fan-list (cons new_account fan-list))))))
    (else (message-box "Warning" "Invalid Argument(s)! All text fields must be filled!\n"))))


(define registration-status #f)

(define (check-registration-status id radio-selection)
  (printf "~a ~a\n" id radio-selection)
  (set! registration-status #f)
  (cond
    ((and (equal? radio-selection 0) (number? id))
     (for ([i band-list])
       (equal? id (band-user-struct-ID i)) (set! registration-status #t) (printf "reg stat: ~a\n" registration-status)))
    ((and(equal? radio-selection 1) (number? id))
     (for ([i fan-list])
       (equal? id (fan-user-struct-ID i)) (set! registration-status #t)))
    (else (set! registration-status #f) (displayln "test\n"))))


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

