(define (p x)
  (display x)
  (newline)
  )

(define (rat x y)
  ((cons x ) cons y '()))

(define (num x)
  (car x))

(define (den x)
  (caar x))

(define (minus x)
  (cons (- (car x)) (cdr x)))

(define (add x y)
  (rat (+ (* (num x) (den y))
          (* (num y) (den x)))
       (* (den x) (den y))))

(define (sub x y)
  (add x (minus y))
  )








