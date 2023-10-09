#lang racket 

#|
Racket Part 1
Developer Name : Neil Mathias
Developer E-mail : neilmathias25@gmail.com
|#

(provide is-digit?)

;;
;1.is-digit?

(define (is-digit? c)
  (if (char? c) 
      (if (and (<= (char->integer c) 57)
               (>= (char->integer c) 48)) 
          #t
          #f
          )
      #f
      )
  )