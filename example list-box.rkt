#lang racket
(require racket/gui/base)

(define frame (new frame% 
                  [label "myTable"]
                  [width 800]
                  [height 600]
                  ))

(define table (new list-box%
                 [parent frame]
                 [choices (list )]
                 [label "Test"]
                 [style (list 'single 'column-headers 'variable-columns)]
                 [columns (list "Name" "Date" "Price")]))
(define data (list 
              (list "Slipknot" "13/05/2025" "25.99$")
              (list "Metallica" "14/06/2025" "30.00$")
              (list "Iron Maiden" "15/07/2025" "40.00$")))


 
(send table set-column-width 0 100 100 300)
(send table set-column-width 1 100 100 300)
(send table set-column-width 2 100 100 300)
(for
    ([i data])
  (send table set (list (list-ref (list-ref data 0) 0)) (list (list-ref (list-ref data 0) 1)) (list (list-ref (list-ref data 0) 2))))
;(apply send table set data) ;--> ERROR: send: bad syntax

;(map (lambda (element)
;             (send table set element)) ;--> ERROR: set in list-box%: column count doesn't match argument count  column count: 3  argument count: 1
;     data)

(send frame show #t)