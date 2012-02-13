#lang scheme/base
;(define (scale-tree tree factor)
;  (cond ((null? tree)  null)
;        ((not (pair? tree)) (* tree factor))
;        (else (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))
;        )
;  )

(define (tree-map tree func)
  (cond ((null? tree) null)
        ((not (pair? tree)) (func tree))
        (else (cons (tree-map (car tree) func) (tree-map (cdr tree) func)))
        )
  )
(define (square x) (* x x))
(define (square-tree tree) (tree-map tree square))
;(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (subsets s)
  (if(null? s)
     (list null)
     (let ([rest (subsets (cdr s))])
       (append rest (map (lambda(x) (cons (car s) x)) rest)))
     )
  )
(subsets '(1 2 3))