#lang scheme/base
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))
        )
  )

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))
        )
  )
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((= (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) (cdr s2))))
        ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
        ((< (car s2) (car s1)) (cons (car s2) (union-set s1 (cdr s2))))
        )
  )
(define a (list 1 4 5 6 8 9))
(define b (list 1 2 3 4 7 10))