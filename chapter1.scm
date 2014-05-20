;; helper 
(define p 
  (lambda (x) 
    (display x)
    (newline)))


(define pi 3.1415)

(define area 
  (lambda (x)
    (* pi x  x)
    ))

(define radius 7.0)

(p (area radius))

;; p7
;; special form
;; (define x 3)
(define (square x) (* x x))
(p (square 20))

(define (abs x)
  (cond ((< x 0) (- x))
    (#t x)
    ))

(p (abs -10))

;; exe 1.2
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) 
     (* 3 (- 6 2) (- 2 7)))

;; exe 1.3
  (define (max x y) 
    (if (> x y) x y))

  (define (min x y)
    (if (< x y) x y))

  (define (max-two a b c)
    (+ (max a b) (max (min a b) c))
    )

  (p (max-two 3 4 5))

;; exe1.4
(define (p) (p))
(define (test x y) x)












  
