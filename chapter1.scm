;; sicp chapter one code and exercise

;; helper for pretty-print
(define p
  (lambda (x) 
    (display x) 
    (newline)))

;; close threshold
(define close-enough 0.0001)

(define (square x) 
  (* x x))

(define (abs x) 
  (if (< x 0) (- x) x))

(define (avg x y)
  (/ (+ x y) 2))

(define (close? guess sq)
  (< (abs (- (square guess) sq)) close-enough))

(define (fac-close? guess sq)
  (< (/ (abs (- (square guess) sq)) sq) close-enough)
  )

(define (improve guess sq)
  (avg guess (/ sq guess)))

(define (sr guess x close-fn)
  (if (close-fn guess x)
    guess
    (sr (improve guess x) x close-fn)
   ))


;; main 

;;(p (abs -100))
;;(p (abs 100))
;;(p (square 10))

(p (sr 1.0 0.0000000001 close?))
(p (sr 1.0 0.0000000001 fac-close?))













