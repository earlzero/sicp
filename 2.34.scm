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
(define (enumerate-interval low high)
  (if (> low high) null (cons low (enumerate-interval (+ 1 low) high)))
  )
;(define (enumerate-tree tree)
;  (cond ((null? tree) null)
;        ((not (pair? tree)) (list tree))
;        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))
;        )
;  )
;(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
;(define (square x) (* x x))
;(define (sum-odd-squares tree)
;  (accumulate + 0 (map square (filter odd? (enumerate-tree tree))))
;  )
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence)
  )
(define (append seq1 seq2)
  (accumulate cons seq2 seq1) 
  )
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms))) 0 coefficient-sequence)
  )
(horner-eval 2 (list 1 3 0 5 0 1))