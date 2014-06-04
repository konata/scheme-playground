(define (p . w)
  (display w)
  (newline))

(define (atom? exp)
  (and (not (pair? exp)) (not (null? exp))))


(define (left exp)
  (car exp))

(define (right exp)
  (caddr exp))

(define (num? exp)
  (cond 
    ((atom? exp) (number? exp))
    (else (let ((left (car exp)) (right (caddr exp)))
            (and (num? left) (num? right))))))

;(p (num? '(2 + (1 + 5))))
;(p (num? '(2 + (1 + a))))

(define (value exp)
  (cond
    ((atom? exp) exp)
    (else ((op exp) (value (left exp)) (value (right exp))))))

(define (op exp)
  (let ((opcode (cadr exp)))
    (cond
    ((eq? '* opcode) *)
    ((eq? '+ opcode) +)
    ((eq? '- opcode) -)
    ((eq? '/ opcode) /)
    )))

(p (value '(1 + (3 * (10 / 2)))))



  




