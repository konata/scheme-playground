;; chapter number-games for the little schemer


(define p 
  (lambda (x) (display x) (newline)))

(define add1 
  (lambda (x) (+ x 1)))

(define sub1
  (lambda (x) (- x 1)))

(define add
  (lambda (x n) 
    (cond
      ((zero? n) x)
      (else (add (add1 x) (sub1 n))))))


;; main 
(p (add1 100))
(p (add 100 20))



(define cur-page
  (lambda () 65))

(p (cur-page))

;; the first commandment
;; when recurring on a list of atoms, lat , two question about it
;; (null? lat) and else
;; when recurring on a list of number, n , two question about it (zero? n) 
;; and else
;;
;;
;; the fourth commandment
;; always change at least one argument while recurring,it must change closer 
;; to the termination,the changing argument must be tested int he termination
;; condition
;; when using cdr, termination is null?
;; when sub1, termination is zero?
