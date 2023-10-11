#lang racket 

#|
Racket Part 1
Developer Name : Neil Mathias
Developer E-mail : neilmathias25@gmail.com
|#

(provide list_len)
(provide contains)
(provide index-of)
(provide my-subset?)
(provide same-set?)
(provide pairs)
(provide my-remove-duplicates)
(provide find-multiples)
(provide sum)
(provide length)
(provide last)
(provide string-length)
(provide maximum)
(provide make-consed)
;;
; length of list
(define (list_len lst)
  (if (empty? lst) 
       0 
       (+ 1 (list_len(rest lst)))
    ))

;;
; does list contain element : boolean

(define (contains x lst)                    
  (cond [(empty? lst)           #f]
        [(equal? x (first lst)) #t]
        [else (contains x (rest lst))]
    ))

;;
; linear search
; returns -1 if element not found in list

(define (index-of x lst)
  (if (contains x lst)
      (cond [(equal? x (first lst)) 0]
        [else (+ 1 (index-of x (rest lst)))])
      -1
    ))

;;
; isSubset
; determines whether list 1 is a subset of list 2

(define (my-subset? lst1 lst2)
  (if (empty? lst1)
      #t
      (if (contains (first lst1) lst2)
          (my-subset? (rest lst1) lst2)
           #f
        )
    ))

;;
; set equality

(define (same-set? lst1 lst2)
  (if (and (my-subset? lst1 lst2) (my-subset? lst2 lst1))
      #t
      #f
    ))

;;
;5.pair two lists

(define (pairs lst1 lst2)
  (cond [(empty? lst1) '()]
        [(empty? lst2) '()]
        [else (cons 
                (cons (first lst1) (first lst2)) 
                (pairs (rest lst1) (rest lst2))
                )]
    ))

;;
; remove duplicates from list

(define (my-remove-duplicates lst)
  (cond [(empty? lst) empty]
        [(contains (first lst) (rest lst)) (my-remove-duplicates (rest lst))]
        [else (cons 
                (first lst) 
                (my-remove-duplicates (rest lst)))]
    ))


;;
; finding multiples

(define (find-multiples lst)
  (cond [(empty? lst) empty]
        [(not (contains (first lst) (rest lst))) (find-multiples (rest lst))]
        [else (my-remove-duplicates (cons (first lst) (find-multiples (rest lst))))]
    ))

;;
; list fold right

(define (foldr f init lst)      
  (if (empty? lst) init
      (f (first lst) (foldr f init (rest lst))))) 

;;
; return sum of numbers in list

(define (sum lst)               
  (foldr + 0 lst))
 
;;
; maximizing a function as per operation
; e.g. (maximum sum list_of_lists) returns the list whose sum is the max

(define (maximumNum f lst)
  (if (empty? lst)
      0
      (max (f (first lst)) (maximumNum f (rest lst)))))
 
(define (maximum f lst) 
  (cond [(empty? lst) 0]
        [(equal? (f (first lst)) (maximumNum f lst)) (first lst)] 
        [else (maximum f (rest lst))])) 

;;
;9.calculating consed-out lists

(define (make-consed x)
  (if (empty? x)
      '()
      (if (list? (first x))
          `(cons (cons ,(first (first x))) ,(make-consed (rest x)))
          `(cons ,(first x) ,(make-consed (rest x))))))

