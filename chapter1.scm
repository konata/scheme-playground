;; sicp chapter one code and exercise

;; helper for pretty-print
(define p
  (lambda (x) 
    (display x) 
    (newline)))

;; current page
(define progress 
  (lambda ()
  "p40"))


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


;; chapter 1.2
(define (fac-recursive) 
  (lambda (n)
    (if (= n 1) 1 (* n (fac (- n 1)))
    )))

(define (fac n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count) product (fact-iter (* product counter) (+ 1 counter) max-count)))
  (fact-iter 1 1 n)
  )




;; main 

(p (sr 1.0 0.0000000001 close?))
(p (sr 1.0 0.0000000001 fac-close?))
(p (if (> 2 1) 100 (_loop)))

;; pass as quote to escape default evaluation rule
;  (p (_if (> 2 3) '100 '(_loop)))

(p (cuberoot 0.0000000000000000009))
(p (progress))

(p (/ (fac 100) (fac 99)))

















