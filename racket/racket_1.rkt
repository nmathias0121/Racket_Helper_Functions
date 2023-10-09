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

'("Tests for subset function")
(my-subset? '() '())
(my-subset? '() '(1 2))
(my-subset? '(2 1 1) '(1 2))
(my-subset? '(2 1 3) '(1 2))
(my-subset? '(a d (3 1)) '(1 a (3 d)))
(my-subset? '((1) (2 2)) '((3 3 3) a (2 2) (1)))

'("Tests for set equality")
(same-set? '(1 2 3) '(3 1 2))
(same-set? '(1 2 3 3) '(2 3 1 1 1 2))
(same-set? '(1 2 3 3) '(2 1 1 1 2))
(same-set? '(3 1) '(1 2 4 3))

'("Tests for pairing two lists")
(pairs '(a b c) '(1 2 3))
(pairs '(a b c) '(1 2))
(pairs '(a b) '(1 2 3))

'("Tests for remove duplicates")
(my-remove-duplicates '(1 2 3 2))
(my-remove-duplicates '(4 4 4))
(my-remove-duplicates '(up up and a way way))

'("Tests for finding multiples")
(find-multiples '(a b c))
(find-multiples '(a b a c))
(find-multiples '(a b a c c a))           
(find-multiples '(a b a c c b))

'("Tests for find maximum in a list")
(maximum sum '((1 2 0 0 2) (9) (2 8 2 -2)))
(maximum length '((1 2 0 0 2) (9) (2 8 2 -2)))
(maximum last '((1 2 0 0 2) (9) (2 8 2 -2)))
(maximum string-length '("one" "two" "three" "four" "five"))
