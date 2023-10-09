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
    (+ 1 (list_len(rest lst)))))

'("Tests for length of list")
(define l (list 1 (list 2 3) 3 5 3))
(list_len l)
(define l1 (list ))
(list_len l1)