#lang racket/gui
(struct band-user-struct (name surname ID password secret-answer acc-type) #:mutable)
(struct fan-user-struct (name surname ID password secret-answer acc-type) #:mutable)
;(struct logged-in-struct (ID acc-type) #:mutable #:transparent)
(struct band-listing-struct(concertID bandID bandName date time venue cost seat) #:mutable)
;(struct band-listings ()
; Band and fan test account will be deleted
(define test-acc(band-user-struct "Ugur" "Ersoy" "123" "123" "test" 0))
(define test-acc2(band-user-struct "Murat" "Ersoy" "345" "345" "test2" 0))
(define fan-test-acc (fan-user-struct "Alice" "Johnson" "6969" "5252" "test" 1))
(define fan-test-acc2 (fan-user-struct "Bob" "Smith" "3152" "3131" "test2" 1))

(define listing-test (band-listing-struct "11636" "123" "Ugur" "13.05.2025" "14:00" "High Street London" "24.99" "30"))
(define listing-test2 (band-listing-struct "12521" "123" "Ugur" "14.05.2025" "14:55" "Street London" "24.92" "15"))
(define listing-test3 (band-listing-struct "12345" "123" "Ugur" "12.05.2025" "15:00" "5igh Street London" "33.99" "20"))

(define listing-test4 (band-listing-struct "23456" "124" "Ugur" "15.05.2025" "16:00" "Central Park NY" "29.99" "25"))
(define listing-test5 (band-listing-struct "34567" "124" "Ugur" "16.05.2025" "17:00" "Madison Square NY" "19.99" "35"))
(define listing-test6 (band-listing-struct "45678" "124" "Ugur" "17.05.2025" "18:00" "Broadway NY" "39.99" "10"))

(define listing-test7 (band-listing-struct "56789" "125" "Nova" "18.06.2025" "19:00" "Hyde Park London" "49.99" "20"))
(define listing-test8 (band-listing-struct "67890" "125" "Nova" "19.06.2025" "20:00" "Piccadilly London" "59.99" "15"))
(define listing-test9 (band-listing-struct "78901" "125" "Nova" "20.06.2025" "21:00" "Oxford Circus London" "69.99" "5"))

(define listing-test10 (band-listing-struct "89012" "126" "Starlight" "25.07.2025" "22:00" "Union Square SF" "49.99" "50"))
(define listing-test11 (band-listing-struct "90123" "126" "Starlight" "26.07.2025" "23:00" "Golden Gate SF" "59.99" "40"))
(define listing-test12 (band-listing-struct "01234" "126" "Starlight" "27.07.2025" "00:00" "Market Street SF" "69.99" "30"))


;(define test-logged-in (logged-in-struct (band-user-struct-ID test-acc) (band-user-struct-acc-type test-acc)))

;;;;;;;;;;;;;;

;band and fan list -> struct type variables will be hold here
(define band-list(list test-acc test-acc2))
(define fan-list(list fan-test-acc fan-test-acc2))
(define logged-in-acc(list))
;(define listed-concerts(list listing-test listing-test2 listing-test3))
(define listed-concerts
  (list listing-test listing-test2 listing-test3 listing-test4 listing-test5 listing-test6 listing-test7 listing-test8 listing-test9 listing-test10 listing-test11 listing-test12))


(define (remove-from-saved id)
  (cond
    ((member id fan-selected-concerts)
     (set! fan-selected-concerts (remove id fan-selected-concerts)))))

(define fan-selected-concerts (list "11636" "12345" "12521" "01234"))

; creates account by invoking band or fan struct. It creates a new list and assigns it to our band or fan-list variable.
; after it is only read-only but you can change what its pointing at (const char list[2]); -> read-only (const char const idf) -> read only + idf cannot hold a different var


;create functions
(define (generate-random-uid) ;
  (random-seed (random 100000)) ; -> random-seed is from GPT,
  (define rand-uid (random 1000000)) ; stackoverflow https://stackoverflow.com/questions/51987116/random-numbers-in-racket
  rand-uid)

(define (create-account name surname id password radiobutton secret-answer)
  (cond
    ((and name surname id password radiobutton)
     (cond
       ((equal? radiobutton 0) (let ((new_account (band-user-struct name surname id password secret-answer 0)))
                                 (set! band-list (cons new_account band-list))))
       ((equal? radiobutton 1) (let ((new_account (fan-user-struct name surname id password secret-answer 1)))
                                 (set! fan-list (cons new_account fan-list))))))
    (else (message-box "Warning" "Invalid Argument(s)! All text fields must be filled!\n"))))

(define (create-concert-listing concertID bandid name date time venue cost seatleft)
  (cond
    ((and bandid name date time venue cost seatleft)
     (set! concertID (number->string (generate-random-uid)))
     (let ((new-concert (band-listing-struct concertID bandid name date time venue cost seatleft)))
       (set! listed-concerts (cons new-concert listed-concerts))))))

;set functions
(define registration-status 0)

(define (check-registration-status id radio-selection)
  (set! registration-status 0)
  (cond
    ((and (equal? radio-selection 0) (number? id))
     (for ([i band-list])
       (equal? id (band-user-struct-ID i)) (set! registration-status 1)))
    ((and(equal? radio-selection 1) (number? id))
     (for ([i fan-list])
       (equal? id (fan-user-struct-ID i)) (set! registration-status 1)))
    (else (set! registration-status 1) (displayln "test\n"))))
(define selected-concert-status 0)
(define (set-selected-concerts tt)
  (set selected-concert-status 0)
  (set! fan-selected-concerts (cons tt fan-selected-concerts)) (set! selected-concert-status 1))

(define (set-seat uid new-seat)
  (let [(flag 0)]
  (for
      ([i listed-concerts])
    (cond
      ((and (equal? uid (band-listing-struct-concertID i)) (equal? (band-listing-struct-bandID i) id-holder))
       (printf "SeatLeft>struct> ~a ~a ~a\n" uid new-seat (band-listing-struct-seat i))
       (cond
         ((equal? (string->number new-seat) 0)
       (set-band-listing-struct-seat!  i "FULLY BOOKED") (set! flag 1)
       (message-box "Information" "Seat List has been updated!"))
       (else
        {set-band-listing-struct-seat! i new-seat} (set! flag 1)
        (message-box "Information" "Seat List has been updated!"))))
      (else (cond ((equal? flag 0) (set! flag 1) (message-box "Warning" "Concert ID and BandID mismatch"))))))))
(define (cancel-concert uid)
  (let ([flag 0])
    (for 
      ([i listed-concerts])
    (cond
      ((and (equal? uid (band-listing-struct-concertID i)) (equal? (band-listing-struct-bandID i) id-holder))
       (set-band-listing-struct-seat! i "0")
       (set-band-listing-struct-cost! i "0")
       (set-band-listing-struct-venue! i "CANCELLED")
       (set-band-listing-struct-date! i "CANCELLED")
       (set-band-listing-struct-time! i "CANCELLED")
       (set-band-listing-struct-concertID! i "CANCELLED")
       (set! flag 1)
       (message-box "Information" "Concert has been cancelled!"))))
    (cond
      ((equal? flag 0)
       (message-box "Warning" "Concert ID and BandID mismatch")))))


(define (set-account-password userid new-pass acc-type)
  (let ([old-password 0])
    (cond
      ((and (equal? acc-type 0) (string? userid))
       (for ([i band-list])
         (cond
           ((equal? userid (band-user-struct-ID i)) (set! old-password (band-user-struct-password i)) (set-band-user-struct-password! i new-pass)
                                                    (message-box "Warning" (format "Old Password: ~a\nNew Paasword: ~a\n Band Name: ~a\nBand ID: ~a/~a" old-password (band-user-struct-password i) (band-user-struct-name i) (band-user-struct-ID i) userid)))))))
    (cond
      ((and (equal? acc-type 1) (string? userid))
       (for ([i fan-list])
         (cond
           ((equal? userid (fan-user-struct-ID i)) (set! old-password (fan-user-struct-password i)) (set-fan-user-struct-password! i new-pass)
                                                   (message-box "Warning" (format "Old Password: ~a\nNew Paasword: ~a\n Fan Name: ~a\nFan ID: ~a/~a" old-password (fan-user-struct-password i) (fan-user-struct-name i) (fan-user-struct-ID i) userid))))))
      (else
       (cond
         ((not (string? userid)) (message-box "Warning" (printf "ID must be a number!"))))))))

(define (set-date-time uid new-date new-time)
  (let ([flag 0])
  (for
      ([i listed-concerts])
    (cond
      ((and (equal? uid (band-listing-struct-concertID i)) (equal? (band-listing-struct-bandID i) id-holder))
       (set-band-listing-struct-date! i new-date)
       (set! flag 1)
       (set-band-listing-struct-time! i new-time)
       (message-box "Information" "Time/Date change(s) has been made!"))))
      (cond
        ((equal? flag 0) (message-box "Warning" "Concert ID and BandID mismatch")))))
         
(define (set-new-price uid new-price)
  (let ([flag 0])
  (for
      ([i listed-concerts])
    (cond
      ((and (equal? uid (band-listing-struct-concertID i)) (equal? (band-listing-struct-bandID i) id-holder))
       (set-band-listing-struct-cost! i new-price) (set! flag 1)
       (message-box "Information" "Price has been changed"))))
    (cond
      ((equal? flag 0)
      (message-box "Warning" "Concert ID and BandID mismatch")))))


(define (set-logged-in-acc st-name)
  (set! logged-in-acc (cons st-name logged-in-acc)))

(define (set-login-empty)
  (set! logged-in-acc '()))

;get functions
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

;cmp functions
(define cmp-uid 0)
(define (compare-ids uid)
    (cond
      ((for
          ([i listed-concerts])
      (and (equal? uid (band-listing-struct-concertID i)) (equal? id-holder (band-listing-struct-bandID i)))
       (set! cmp-uid 1)))
      (else (message-box "Warning" "BandID - ConcertID mismatch"))))


(provide (all-defined-out))

