#lang racket
; Indexed, allowing constant-time access to elements.
; Fixed length upon creation but mutable.
;Why Use Vectors?

;A game board benefits from constant-time access (e.g., checking a specific cell).
;Mutability supports real-time updates.
;Why Not Use Other Structures?

;List: Requires traversing to access elements, making it less efficient for grid-like structures.
;Set: No ordering or indexing capabilities, making it unsuitable for grid representation.

(define board (vector 'X 'O 'X 'O 'X 'O 'X 'O 'X))
(vector-set! board 4 'O) ; Mutate the board to change the middle value
(displayln board) ; Output: '#(X O X O O O X O X)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Unordered collection of unique elements.
;Efficient membership checking.
;Immutable (with Racket’s set module).

;Why Use Sets?

;Ensures uniqueness of elements automatically.
;Efficient for operations like union, intersection, and membership checks.
;Why Not Use Other Structures?

;List: Does not enforce uniqueness, leading to redundant elements.
;Vector: Not designed for membership or uniqueness checks.
(require racket/set)
(define words (list "hello" "world" "hello" "racket"))
(define unique-words (set "hello" "world" "hello" "racket"))
(displayln unique-words)
(displayln words); Output: '#{hello world racket}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Lists


;Ordered and linear.
;Can hold elements of different types.
;Suitable for recursive algorithms.
;Dynamic in size but not random-access efficient.

(define shopping-list '(milk eggs bread))
(define updated-shopping-list (cons 'butter shopping-list))
(displayln updated-shopping-list) ; Output: (butter milk eggs bread)


;Why Use Lists?

;A shopping list is naturally sequential and dynamic. Lists allow easy addition of items (like butter) to the front.
;Traversal is efficient when reviewing all items.
;Why Not Use Other Structures?

;Vector: Not ideal for frequently adding items; vectors have fixed lengths and require resizing.
;Set: Shopping lists may intentionally have duplicates (e.g., buying two cartons of milk), which sets would eliminate. Additionally, sets don’t maintain order.



