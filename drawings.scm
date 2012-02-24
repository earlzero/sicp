#lang scheme/base
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define wave2 (beside einstein (flip-vert einstein)))
(define wave4 (below wave2 wave2))
;(define (flipped-pairs painter)
;  (let ([painter2 (beside painter (flip-vert painter))])
;    (below painter2 painter2)
;    )
;  )
;(define (right-split painter n)
;   (if (= n 0)
;       painter
;       (let ([smaller (right-split painter (- n 1))])
;         (beside painter (below smaller smaller))
;         )
;       )
;   )
;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ([smaller (right-split painter (- n 1))])
;        (below painter (beside smaller smaller))
;        )
;      )
;  )

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let* ([up (up-split painter (- n 1))]
             [right (right-split painter (- n 1))])
        (let* ([top-left (beside up up)]
               [bottom-right (below right right)]
               [corner (corner-split painter (- n 1))])
          (beside (below painter top-left) (below bottom-right corner))
          )
        ))
  )
(define (square-of-four tl tr bl br)
  (lambda(painter) (let ([top (beside (tl painter) (tr painter))]
                         [bottom (beside (bl painter) (br painter))])
                     (below bottom top))
    ))
(define (identity l) l)  
(define (flipped-pairs painter)
  (let ([combine4 (square-of-four identity flip-vert identity flip-vert)])
    (combine4 painter))
  )
(define (square-limit painter n)
  (let ([combine4 (square-of-four flip-horiz identity rotate180 flip-vert)])
    (combine4 (corner-split painter n))
    )
  )
(define (split op1 op2)
  (define (split-util painter n)
       (if (= n 0)
       painter
       (let ([smaller (split-util painter (- n 1))])
         (op1 painter (op2 smaller smaller))
         )
       )
  )
    (lambda(painter n) (split-util painter n))
)

(define right-split (split beside below))
(define up-split (split below beside))


    