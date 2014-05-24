(define (p x)
  (display x)
  (newline))

(define zero (lambda (f) (lambda (x) x)))

(define (add1 n)
  (lambda (f) (lambda (x) (f (n f) x))))


(ddd 1 zero)
 = (lambda (f)) (lambda (x) (f (zero f) x))



