
;; helper
(define (p x)
  (display x)
  (newline)
  )

(define (error reason)
  (display "Error:")
  (display reason)
  )

;; main calculator
(define calc
  (lambda (exp)
    (cond 
      ((number? exp) exp)
      ((list? exp) (reduce-token exp))
      (else (display "ErrorToken:") (display exp)))))


(define (reduce-token exp)
  (let ((op (car exp)) (first (cadr exp)) (remain (cddr exp)))
    (p first)
    (p remain)
    ;;(p op)
    ;;(p (eq? op '+))
  
    (cond
      ((eq? op '+) (add first remain))
      ((eq? op '-) (minus first remain))
      ((eq? op '*) (mul first remain))
      ((eq? op '/) (div first remain))
      (else (display "ErrorToken:")))))

(define (add first remain)
  (cond
    ((null? remain) first)
    (else (add (+ first (car remain) (cdr remain))))
  ))

(define (minus first remain)
  (cond
    ((null? remain) first)
    (else (minus (- first (car remain) (cdr remain))))
  ))

(define (mul first remain)
  (cond
    ((null? remain) first)
    (else (mul (* first (car remain) (cdr remain))))
  ))

(define (div first remain)
  (cond
    ((null? remain) first)
    (else (div (/ first (car remain) (cdr remain))))
  ))


;; should recursive calculate sub values
(p (calc '(+ 1 3)))
(p (calc '(+ 1 3 4 (* 5 7) (- 10 5) (/ 10 2))))







