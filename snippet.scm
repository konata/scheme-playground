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

  ;; Q 1.33
  (define (filter combiner null-value term cur next end predication greater)
    (cond 
      ((greater cur next) null-value)
      ((predication cur) (combiner (term cur) (filter combiner null-value term (next cur) next end predication greater)))
      (else (filter combiner null-value term (next cur) next end predication greater))))

  ;; using inner helper
  (define (f x y)
    (define (f-helper a b)
      (+ (* x (square a)))
        (* y b)
        (* a b))
    (f-helper (+ 1 (* x y))
                (- 1 y)))

  ;; using lambda
  (define (f x y)
    ((lambda (a b)
       (+ (* x (square a))
          (* y b)
          (* a b)))
       (+ 1 (* x y))
       (- 1 y)))

  ;; using let
  (define (f x y)
    (let ((a (+ 1 (* x y))) 
      (b (- 1 y)))
      (+ (* x (square a))
         (* y b)
         (* a b))))
  )

  ;; (let ((a x) (b y))  (body))
  ;; == 
  ;; ((lambda (a b) (body)) x y)


  ;; Q1.34
  (begin
      (define (f g)
       (g 2))
      (p (f square))
      (p (f (lambda (x) (* x (+ 1 x)))))
      ;(p (f f))
  )


  (begin
    (define (close x y)
      (< (abs (- x y)) 0.000000000001))
  
    (define (avg x y)
      (/ (+ x y) 2))
  
    (define (search f pos neg)
      (let ((mid (avg pos neg)))
        (cond
          ((close pos neg) mid)
          (else (let ((mid-value (f mid)))
                  (cond 
                    ((= 0 mid-value) mid)
                    ((< 0 mid-value) (search f mid neg))
                    (else (search f pos mid))
                    ))))))
    
    (define (f x)
      (- (* x x x) (* x x ) 1.2))

    (define (pi x)
      (sin x)
      )

    (p (search pi 2.0 4.0))
   )

  ;;fixed-point
  (begin 
    (define tolerance 0.0001)

    (define (close x y)
      (< (abs (- x y)) tolerance))

    (define (fixed-point f guess)
      (let ((next (f guess)))
        (if (close guess next) next (fixed-point f next))))
    
    (p (fixed-point cos 1.0))
    (p (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))

    (define (avg x y) (/ (+ x y) 2))

    (define (sqrt x)
      (fixed-point (lambda (y) (avg y (/ x y))) 1.0))
    
    (p (sqrt 3))
    
    )


  
  








