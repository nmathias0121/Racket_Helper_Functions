#lang racket 

#|
Racket Part 1
Developer Name : Neil Mathias
Developer E-mail : neilmathias25@gmail.com
|#

(provide is-digit?)
(provide is-letter?)

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

;;
;2.isletter?

(define (is-letter? c)
  (if (char? c) 
      (if (or (and (<= (char->integer c) 122)
                   (>= (char->integer c) 97))
              (and (<= (char->integer c) 90)
                   (>= (char->integer c) 65))) 
          #t
          #f
          ) 
      #f
      )
  )