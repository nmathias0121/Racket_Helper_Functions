#lang racket 

#|
Racket Propositions
Developer Name : Neil Mathias
Developer E-mail : neilmathias25@gmail.com
|#

(require "list_operations.rkt")
(provide is-digit?)
(provide is-letter?)
(provide is-vble?)
(provide is-bool-env?)
(provide is-expr?)
(provide eval-prop)
(provide get-vbles)
(provide all-truth-values)
(provide find-sat-env)

;;
; is-digit?

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
; isletter?

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

;;
; is-vble?
; does the parameter qualify as a eligible variable name

(define (is-vble? x)
  (if (symbol? x) 
      (let ([strx (symbol->string x)])
        (if (is-letter? (string-ref strx 0))   
            (if (not (contains x '(not and or t f))) 
                (if (not (contains #\- (string->list strx))) 
                    #t
                    #f
                    )
                #f
                )
            #f
            )
        )
      #f
      )
  )

;;
; check if lst satisfies conditions for boolean environment (helper function)
(define (bool-env lst)
  (if (list? lst)
      (if (is-vble? (first lst))
          (if (or (equal? (first (rest lst)) 't)
                  (equal? (first (rest lst)) 'f)) 
              #t
              #f
              )
          #f
          )
      #f
      )
  )

;;
; is-bool-env?

(define (is-bool-env? x)
  (if (list? x)
      (if (empty? x)
          #t
          (andmap bool-env x)
          ) 
      #f
      )
  )

;;
;check if list satisfies conditions for expression (helper function)

(define (bool-expr? lst)
  (if (list? lst) 
      (if (empty? lst)
          #t
          (if (list? (first lst))
              (bool-expr? (first lst))
              (if (or (is-vble? (first lst))
                      (or (contains (first lst) '(not and or t f))
                          (equal? (first lst) '-->)))
                  (bool-expr? (rest lst)) 
                  #f
                  )
              )
          )  
      (if (or (is-vble? lst)
              (or (contains lst '(not and or t f))
                  (equal? lst '-->)))
          #t 
          #f
          )
      )
  )

;;
; is-expr?

(define (is-expr? e)
  (if (list? e)
      (if (empty? e)
          #f
          (if (andmap is-vble? e)
              #f
              (andmap bool-expr? e)
              )
          ) 
      (if (or (is-vble? e) (or (equal? e 't) (equal? e 'f)))
          #t
          #f
          )
      )
  )

;;
; determine truth value of variable as per bool env. (helper function)

(define (value k lst)
  (if (equal? k (first (first lst)))
      (first (rest (first lst)))
      (value k (rest lst))
      )
  )

;;
; determine if k is boolean(t/f) (helper function)

(define (is-bool? k)
  (if (or (equal? k 't) (equal? k 'f))
      #t
      #f
      )
  )

;;
; replace variables in list with respective truth value (helper function)

(define (convert-var e env)
  (if (empty? e)
      '()
      (if (list? (first e))
          (append (list (convert-var (first e) env)) (convert-var (rest e) env))
          (if (or (contains (first e) '(and not or -->))
                  (or (contains (first e) (map first env))
                      (is-bool? (first e))))
              (if (contains (first e) (map first env)) 
                  (cons (value (first e) env) (convert-var (rest e) env))
                  (cons (first e) (convert-var (rest e) env))
                  )
              (error 'eval-prop "undefined ~a" "variable")
              )
          )
      )
  )

;;
; solve expression (input lst - list consist of t/f or expressions(and or not -->) values) (helper function)

(define (solve-expr lst)
  (if (empty? lst)
      '()
      (if (ormap list? lst)
          (if (list? (first lst))
              (if (empty? (rest lst))
                  (cons (solve-expr (first lst)) (solve-expr (rest lst)))
                  (solve-expr (cons (solve-expr (first lst)) (solve-expr (rest lst))))
                  )
              (if (contains (first lst) '(and or -->))
                  (cons (first lst) (solve-expr (rest lst)))
                  (solve-expr (cons (first lst) (solve-expr (rest lst))))
                  )
              )
          (if (contains (first lst) '(and or -->))
              lst
              (cond [(equal? (car lst) 'not)
                     (if (equal? (car (cdr lst)) 't) 'f 't)]
                    [(equal? (car (cdr lst)) 'and)
                     (if (equal? (car lst) (car (cdr (cdr lst)))) (car lst) 'f)]
                    [(equal? (car (cdr lst)) 'or)
                     (if (not (equal? (car lst) (car (cdr (cdr lst))))) 't (car lst))]
                    [(equal? (car (cdr lst)) '-->)
                     (if (equal? (car lst) (car (cdr (cdr lst)))) 't (car (cdr (cdr lst))))] 
                    )
              )
          )
      )
  )

;;
; eval-prop

(define (eval-prop e env)
  (if (list? e)
      (solve-expr (convert-var e env))
      (cond [(is-bool? e) e]
            [(contains e (map first env)) (value e env)] 
            [else (error 'eval-prop "undefined ~a" "variable")]
            )  
      )
  ) 

;;
; return list of all variables (helper function)

(define (vbles-lst e)
  (if (list? e)
      (if (empty? e)
          '()
          (if (list? (first e))
              (flatten (cons (vbles-lst (first e)) (vbles-lst (rest e))))
              (if (is-vble? (first e))
                  (cons (first e) (vbles-lst (rest e)))
                  (vbles-lst (rest e))
                  )
              )
          )
      (if (is-vble? e)
          (list e)
          '()
          )
      )
  )

;;
; remove duplicates (helper function)

(define (elim-dupl lst)
  (cond [(empty? lst) empty]
        [(contains (first lst) (rest lst)) (elim-dupl (rest lst))]
        [else (reverse (cons (first lst) (elim-dupl (rest lst))))]
        )
  )

;;
; get-vbles
; display all variables in a proposition

(define (get-vbles e)
  (elim-dupl (vbles-lst e))
  )

;;
; implementation of append map (helper function)

(define (app-map func lst)
  (if (empty? lst)
      '()
      (append (func (first lst))
            (app-map func (rest lst)))
      )
  )
  
;;
; all-truth-values
; display all combinations of truth values

(define (all-truth-values n)
  (if (equal? n 0)
      '(())
      (app-map (lambda (a)
                   (map (lambda (b)
                     (cons b a))
                        '(t f))) 
            (all-truth-values (- n 1)))
      )
  )

;;
; check if k expression is solved (no variable) (helper function)

(define (is-solved? k)
  (if (or (contains k '(not and or -->)) (is-bool? k))
      #t
      #f
      )
  )

;;
; map all variables with '(t f) (helper function)

(define (raw-env e n)
  (if (equal? n 0)
      '(())
      (app-map (lambda (a) 
                   (map (lambda (b)
                     (cons b a))
                        (append (get-vbles e) '(t f)) 
                        )
                 ) 
            (raw-env e (- n 1))
            )    
      ) 
  )

;;
; filter (variable truth-value) pairs (helper function)

(define (filter-raw-env lst)
  (cond [(empty? lst) '()]
        [(equal? (or (is-bool? (first (first lst)))
                     (is-vble? (first (rest (first lst))))) #t)
         (filter-raw-env (rest lst))]
        [else
         (cons (first lst) (filter-raw-env (rest lst)))]
        )
  )

;;
; map all raw mappings (helper function)

(define (processed-env e n)
  (if (equal? n 0)
      '(())
      (app-map (lambda (a) 
                   (map (lambda (b)
                     (cons b a))
                        (filter-raw-env (raw-env e 2)) 
                        )
                 ) 
            (processed-env e (- n 1))
            )     
      ) 
  )

;;
; remove ((variable truth-value) (variable truth-value)) pairs (helper function)

(define (filter-processed-env lst)
  (cond [(empty? lst) '()]
        [(equal? (length (get-vbles (flatten (first lst)))) 1) 
         (filter-processed-env (rest lst))]
        [else
         (cons (first lst) (filter-processed-env (rest lst)))]))

;;
; final environment with (variable truth-value) satisfying boolean environment (helper function)

(define (final-env e)
  (reverse (filter (lambda (env)
            (equal? (solve-expr (convert-var e env)) 't)) (filter-processed-env (processed-env e 2)))))

;;
; find-sat-env
; values of variables (true or false) such that the proposition evaluates to true
; if proposition is by default false, then return 'unsat'

(define (find-sat-env e)
  (cond [(or (empty? e) (equal? e 'f)) 'unsat]
        [(equal? e 't) 't]
        [(is-vble? e) (list (cons e '(t)))]
        [(and (list? e) (andmap is-solved? (flatten e))) (if (equal? (solve-expr e) 't) 't 'unsat)] 
        [else (if (empty? (final-env e))
                  'unsat
                  (first (final-env e)))]))

