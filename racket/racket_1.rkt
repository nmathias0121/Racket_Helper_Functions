#lang racket 

#|
Racket Part 1
Developer Name : Neil Mathias
Developer E-mail : neilmathias25@gmail.com
|#

;;
; length of list
(define (list_len lst)
  (if (empty? lst) 
       0 
       (+ 1 (list_len(rest lst)))
    ))

;;
; linear search
; returns -1 if element not found in list

(define (contains x lst)                    ; does list contain element : boolean
  (cond [(empty? lst)           #f]
        [(equal? x (first lst)) #t]
        [else (contains x (rest lst))]
    ))

(define (index-of x lst)
  (if (contains x lst)
      (cond [(equal? x (first lst)) 0]
        [else (+ 1 (index-of x (rest lst)))])
      -1
    ))

'("Tests for length of list")
(define list_1 (list 1 (list 2 3) 3 5 3))
(list_len list_1)
(define empty_list (list ))
(list_len empty_list)

'("Tests for linear search")
(index-of 1 list_1)
(index-of '(2 3) list_1)
(index-of 3 empty_list)
(index-of 5 list_1)
(index-of 16 '(1 4 9))