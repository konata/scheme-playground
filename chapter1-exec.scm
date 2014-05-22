;; pretty print utility
(define (p x)
  (display x)
  (newline))

(define (page) 54)


(define (fast-expt b n)
  (cond 
    ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (*b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))


;; Q1.11
;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) f(n) = n if n < 3
;; recursion version
(define (f n)
    (cond 
      ((< n 3) n)
      (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))
;(p (f 25))


;; iteration version
(define (f n)
  (define (fn fn_1 fn_2 fn_3 num n)
    (cond
      ((< n 3) n)
      ((= (+ num 1) n) (+ fn_1 (* 2 fn_2) (* 3 fn_3)))
      (else (fn (+ fn_1 (* 2 fn_2) (* 3 fn_3)) fn_1 fn_2 (+ 1 num) n))))
  (fn 2 1 0 2 n))

;(p (f 25))

;; Q1.12 
(define (pascal x y)
  (if (or (= x 1) (= x y)) 
    1 
    (+ (pascal (- x 1) (- y 1))  (pascal x (- y 1)))))

(p (pascal 3 5))

;; Q1.17
(define (* a b)
  (if (= b 0) 0 (+ a (* a (- b 1))))
  )

(p (* 10 9))


;; Q1.17 log complexity
(begin 
  ;; double
  (define (double x)
    ;(* x 2))
    (+ x x))
    
  ;; even number prediction
  (define (even? n)
    (= (remainder n 2) 0))
    
  ;; half
  (define (halve x)
    (/ x 2))
  
  (define (* a b)
    (cond 
      ((= 1 a) b)
      ((even? a) (double (* (halve a) b)))
      (else (+ b (* (- a 1) b)))))
  
  (p (* 10 9)))



;; Q1.19
(begin
(define (gcd a b)
  (if (= b 0)
    a (gcd b (remainder a b))))
(p (gcd 100 30))
)

;; smallest divisor
(begin
  (define (div? a b)
    (= (remainder b a) 0))
  (define (div n guess)
    (cond 
      ((> (square guess) n) n)
      ((div? guess n) guess)
      (else (div n (+ guess 1)))
    )
  )
  (define (sdiv m)
    (div m 2))

  (p (sdiv 199))
  (p (sdiv 1999))
  (p (sdiv 19999))
)


;; Q1.30
(begin
  (define (sum term a next b)
    (if (> a b) 0 (+ (term a) (sum term (next a) next b))))
  (define (term x)
    (* x x))
  (define (next x)
    (+ 1 x))
  (p (sum term 1 next 10)))


(begin 
  (define (sum term a next b result)
    (if (> a b) result 
      (sum term (next a) next b (+ result (term a)))))
    (define (term x)
      (* x x))
    (define (next x)
      (+ 1 x))
    (p (sum term 1 next 10 0)))


;; Q1.31
(begin
  (define (next n)
    (+ 2 n))

  (define (term n)
    (/ (* (- n 1) (+ n 1)) (* n n)))

  (define (product term a next b result)
    (if (> a b) (* 4 result)
      (product term (next a) next b (* result (term a)))))

  (p (product term 3.0 next 100001.0 1.0)))








