#lang scheme/base
(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence) (accumulate op initial (cdr sequence)))
      )
  )
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))
      )
  )
(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(define (vector-mul-util v w)
  (accumulate-n * 1 (list v w))
  )
(define (dot-product v w)
  (accumulate + 0 (map * v w))
)
(define (matrix*vector m v)
  (map (lambda(s) (dot-product s v)) m)
  )
(define (transpose m)
  (accumulate-n cons null m)
  )
(define (matrix*matrix m n)
  (let ([cols (transpose n)])
    (map (lambda(x) (matrix*vector n x)) m)
    )
  )
(define m '((1 3) (1 2)))
(define m3 '((1 2 3) (4 5 6) (7 8 9)))
(define ee '((1 0 0) (0 2 0) (0 0 2)))
(define v '(2 3))

(matrix*matrix m3 ee)