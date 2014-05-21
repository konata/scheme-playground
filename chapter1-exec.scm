;; pretty print utility
(define (p x)
  (display x)
  (newline))

(define (page) 54)

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

;; Q1.13










