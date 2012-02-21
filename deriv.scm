#lang scheme/base
(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence) (accumulate op initial (cdr sequence)))
      )
  )
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr 
                                                                            sequence))))
        (else (filter predicate (cdr sequence)))
        )
  )
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
;(define (make-sum a1 a2) (list '+ a1 a2));first version
(define (=number? exp num)
  (and (number? exp) (= exp num))
  )
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;       ((and (number? a1) (number? a2)) (+ a1 a2))
;        ((and (sum? a1) (sum? a2)) (cons '+ (append (cdr a1) (cdr a2))))
;        ((and (sum? a1) (number? a2)) (cons '+ (cons a2 (cdr a1))))
;        ((and (number? a1) (sum? a2)) (make-sum a2 a1))
;        (else (list '+ a1 a2))
;        )
;)
(define (make-sum a . r)
(define (make-sum-util a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list ’+ a1 a2))))
(append make-sum a r))

(define x (make-sum 1 'x))
(define y (make-sum 1 'y))
(define z (make-sum x y))
;(define (make-product m1 m2) (list '* m1 m2));first version
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))
        )
  )
(define (make-exponentiation b e)
  (cond ((=number? b 1) 1)
        ((=number? e 1) b)
        (else (list '^ b e))
        )
  )
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^))
  )
(define (base e)
  (cadr e)
  )
(define (exponent e)
  (caddr e)
  )
(define (addend s) (cadr s))
(define (augend s) 
  (if (> (length s) 3) (cons '+ (cdr (cdr s)))
      (caddr s)
      )
  )
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                                  (make-product (deriv (multiplier exp) var) (multiplicand exp))))
        ((exponentiation? exp) (make-product (make-product (exponent exp) 
                                                           (make-exponentiation 
                                                            (base exp) 
                                                            (make-sum (exponent exp) (- 1)))
                                                           ) (deriv (base exp) var)))
        (else (error "unknown expression type - DERIVE" exp))
        )
  )
