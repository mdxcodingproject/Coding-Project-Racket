#lang racket/gui
(require "registerwindow.rkt")
(require "structs.rkt")
(define (find-secret-answer user-secret acc-type)
  (let ([flag 0])
    (cond
      ((equal? acc-type 0)
       (for ([i band-list])
         (cond
           ((equal? user-secret (band-user-struct-secret-answer i)) (printf "SA: ~a\n" (band-user-struct-secret-answer i)) (set! flag 1))))))
    (cond
      ((equal? acc-type 1)
       (for ([i fan-list])
         (cond
           ((equal? user-secret (fan-user-struct-secret-answer i)) (printf "SA: ~a\n" (fan-user-struct-secret-answer i)) (set! flag 1))))))
    (cond
      ((equal? flag 0) (display "no secret answer\n")))))



(define (password-reset)
  (define password-reset-frame (new frame% [label "Password Reset"] [width 300] [height 200]))
  (define passresset-radio-button (new radio-box% [parent password-reset-frame] [label ""] [choices (list "Band" "Fan")]))
  (define secret-answer-text-field (new text-field% [parent password-reset-frame] [label "Secret Answer"]))
  (define request-reset-button (new button% [parent password-reset-frame] [label "Request Reset"] [callback (lambda (button event)
                                                                                                              (let ([user-secret-ans (send secret-answer-text-field get-value)]
                                                                                                                    [acc-type (send passresset-radio-button get-selection)])
                                                                                                                (printf "Ans: ~a Acc-type: ~a\n" user-secret-ans acc-type)
                                                                                                                (find-secret-answer user-secret-ans acc-type)))]))
                                                                                                                   
  



  
  (send password-reset-frame show #t))

(provide (all-defined-out))