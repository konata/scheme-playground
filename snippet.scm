(define (p x) 
  (display x)
  (newline)
  )

(begin
  (define (next n)
    (+ 2 n))

  (define (term n)
    (/ (* (- n 1) (+ n 1)) (* n n)))

  ;; Q1.31 iterate
  (define (product term a next b result)
    (if (> a b) (* 4 result)
      (product term (next a) next b (* result (term a)))))

  (p (product term 3.0 next 100001 1))
  
  ;; Q1.31 recursive
  (define (pi cur last)
    (if (< last cur) 4 (* (term cur) (pi (next cur) last))))

  (p (pi 3.0 1001))
  
  ;; Q1.32 accumulate
  (define (accumulate combiner null-value term cur next end greater)
    (if (greater cur end) 
      null-value
      (combiner (term cur) (accumulate combiner null-value term (next cur) next end greater)
      )))
  
  (p (accumulate + 0 (lambda (x) (* x x)) 1 (lambda (x) (+ x 1)) 3 >))

  ;;Q 1.33
  (define (filter combiner null-value term cur next end predication greater)
    (cond 
      ((greater cur next) null-value)
      ((predication cur) (combiner (term cur) (filter combiner null-value term (next cur) next end predication greater)))
      (else (filter combiner null-value term (next cur) next end predication greater))))




  
  )














