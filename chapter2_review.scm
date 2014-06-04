;; utility
(define (p lat) 
  (display lat)
  (newline)
  )

(define (err)
  (p "ERROR"))


;;Q2.4
;(define (cons x y)
;  (lambda (m) (m x y)))
;
;(define (car z)
;  (z (lambda (p q) p)))
;
;(define (cdr z)
;  (z (lambda (p q) q)))
;
;(p (cdr (cons 1 2)))
;(p (car (cons 4 (cons 3 (cons 1 2)))))

(define (exp base idx)
  (cond 
    ((= idx 1) base)
    (else (* base (exp base (- idx 1))))))

(define (ln base radix)
  (if (and (not (= 0 base)) (= (remainder radix base) 0))
    (+ 1 (ln base (/ radix base)))
    0))

(define (_cons x lat)
  (* (exp 2 x) (exp 3 lat)))

(define (_car lat)
  (ln 2 lat))

(define (_cdr lat)
  (ln 3 lat))


;;Q2.6
(define (zero f) 
  (lambda (x) x))

(define (add n)
  (lambda (f)
    (lambda (x) (f ((n f) x)))))

;;one
;;(add zero) = 
;;(lambda (f) (lambda (x) (f (f x))))
;;
;;(add one) = 
;;(lambda (f) (lambda (x) (f (f (f x)))))

;;Q2.17
(define (last-pair lat)
  (cond
    ((null? lat) (err))
    ((null? (cdr lat)) (car lat))
    (else (last-pair (cdr lat)))))

(p (last-pair (list 1 2 3 4 5 6 7)))
(p (last-pair '()))

;;Q2.20
(define (filter pred lat)
  (cond
    ((null? lat) '())
    ((pred (car lat)) (cons (car lat) (filter pred (cdr lat))))
    (else (filter pred (cdr lat)))))

(define (same-parity first . remain)
  (cond
    ((null? remain) first)
    ((even? first) (filter even? remain))
    (else (filter odd? remain))))

(define all-parity 
  (lambda (w . g)
    (p w)
    (p g)))

(p (same-parity   4 5 6 7 8 9 10))

(all-parity 3 4 5 6 7 8 9)


(define square 
  (lambda (num)
    (* num num)
    ))

(define (square-list . lat)
  (if (null? lat) '() (map square lat)))

(p (square-list 1 2 3 4 5 6 ))




(define (count-leaves x)
  (cond ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))


;;Q2.27
(define x (list (list 1 2) (list 3 4)))

(define (reverse x bucket) 
  (if (null? x) bucket (reverse (cdr x) (cons (car x) bucket))))

(p (reverse x '()))

(define (deep-reverse x bucket)
  (if (null? x) bucket (reverse (cdr x) (cons (car x) bucket))))


(define (accumulate fn init lat)
  (if (null? lat) init
    (accumulate fn (fn (car lat) init) (cdr lat))))


;; filter map accumulate
(define (honer-eval x co)
  (accumulate (lambda (this-co higher-term)
                (* (+ this-co higher-term) x))
                0 co))

(p (honer-eval 2 (list 1 1)))



(define (queens border-size)
  
  
  
  )
























