;; sicp chapter one code and exercise

;; helper for pretty-print
(define p
  (lambda (x) 
    (display x) 
    (newline)))

;; current page
(define progress 
  (lambda ()
  "p53"))


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

;; how to invoke list as default evaluation ?
(define (_if _pred _then _else)
  (cond 
  (_pred _then)
  (else _else)))

(define (_loop) (_loop))


;; exec 1.8
(define (_cuberoot guess cube close improve)
  (if (close guess cube) 
    guess 
    (_cuberoot (improve guess cube) cube close improve)
  ))

(define (cuberoot x) 
  (_cuberoot 1.0 x 
             ;; close predication
             (lambda (guess cube) 
               (< (/ (abs (- (* guess guess guess) cube)) cube) close-enough))
                
             ;; improve predication
             (lambda (guess cube) 
               (/ (+ (/ cube (square guess)) (* 2 guess)) 3)
                )))


;; chapter 1.2 recursive process
(define (fac-recursive) 
  (lambda (n)
    (if (= n 1) 1 (* n (fac-recursive (- n 1)))
    )))

;; interactive process
(define (fac n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count) product (fact-iter (* product counter) (+ 1 counter) max-count)))
  (fact-iter 1 1 n))



;; exec 1.10
(define (ack x y)
  (cond 
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (ack (- x 1) (ack x (- y 1))))))

(define (ack-f n) 
  (ack 0 n))

(define (ack-g n)
  (ack 1 n))

(define (ack-h n)
  (ack 2 n))

(define (ack-k)
  (* 5 n n))


(define (fib1 n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib1 (- n 1)) (fib2 (- n 2))))))

(define (fib2 n)
  (define (fib-iter a b count)
    (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))




 
(define (money-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond 
    ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else (+ (cc amount (- kinds-of-coins 1))
             (cc (- amount (money-value kinds-of-coins)) kinds-of-coins)))))

(define (money-value dimen)
  (cond
    ((= dimen 1) 1)
    ((= dimen 2) 5)
    ((= dimen 3) 10)
    ((= dimen 4) 25)
    ((= dimen 5) 50)
    ))

  


;; main 

(p (sr 1.0 0.0000000001 close?))
(p (sr 1.0 0.0000000001 fac-close?))
(p (if (> 2 1) 100 (_loop)))

;; pass as quote to escape default evaluation rule
;  (p (_if (> 2 3) '100 '(_loop)))

(p (cuberoot 0.0000000000000000009))
(p (progress))

(p (/ (fac 100) (fac 99)))

(p (ack 1 10))
(p (ack 2 4))
(p (ack 3 3))

;; (* 2 n)
(p (ack-f 10))

(p (fib1 100))
(p (fib2 100))
(p (money-change 100))


















