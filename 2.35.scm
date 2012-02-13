#lang scheme/base
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))
        )
  )
(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence) (accumulate op initial (cdr sequence)))
      )
  )
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))
        )
  )


;(define (count-leaves t)
;  (accumulate (lambda(c r) (+ 1 r)) 0 (map (lambda(x) x) (enumerate-tree t)))
;  )

(define (count-leaves tree)
  (accumulate +
              0 
              (map (lambda(t) 
                               (if(pair? t) (count-leaves t)
                                  1
                                  )
                               )
                   tree)
              )
  )
(count-leaves '( 1 (2 (3)  4 (5 6 (7 (8 9))))))