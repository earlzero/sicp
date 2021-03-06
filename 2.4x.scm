#lang scheme/base
(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence) (accumulate op initial (cdr sequence)))
      )
  )
(define (flatmap proc seq)
  (accumulate append null (map proc seq))
  )
(define (enumerate-interval low high)
  (if (> low high) null (cons low (enumerate-interval (+ 1 low) high)))
  )


(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))
        )
  )

(define (unique-pairs n)
  (flatmap (lambda(i) (map (lambda(j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))
  )
(define (unique-triples n)
  (flatmap (lambda(i) (map (lambda(j) (cons i j)) (unique-pairs (- i 1)))) (enumerate-interval 1 n))
  )
(define (sum l)
  (accumulate + 0 l)
  )
(define (sum-k-s k s)
  (filter (lambda(x) (= (sum x) s)) (unique-triples k))
  )
(sum-k-s 9 9)

