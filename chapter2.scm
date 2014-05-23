(define (p x)
  (display x)
  (newline)
  )

;; normal operators

;(define rat cons)
(define num car)
(define den cdr)

(define (gcd x y)
  (if (= y 0) x (gcd y (remainder x y))))

(define (rat x y)
  (let ((divisor (gcd x y)) (att (if (> (* x y) 0) + -)))
    (cons (att (/ x divisor)) (/ y divisor))))


(define (add-rat x y)
  (rat (+ (* (num x) (den y))
          (* (num y) (den x)))
       (* (den x) (den y))))

(define (sub-rat x y)
  (rat (- (* (num x) (den y)) 
          (* (num x) (den y)))
       (* (den x) (den y))))

(define (mul-rat x y)
  (rat (* (num x) (den y))
       (* (den x) (num y))))

(define (div x y)
  (rat (* (num x) (den y))
       (* (den x) (num y))))

(define (rat-eq? x y)
  (= (* (num x) (den y)) (* (num y) (den x))))

(define (print rat)
  (newline)
  (display (num rat))
  (display "/")
  (display (den rat)))

;; main
(define one-half (rat 4 -104))
(define one-third (rat 20 -40))

(print (div one-third one-half))








