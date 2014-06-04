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



(define lat '(this is an nice item nice item an nice item))
  
;; multiremeber
(define (multiremeber a lat col)
  (cond 
    ((null? lat) (col '() '()))
    ((eq? (car lat) a) (multiremeber a (cdr lat) 
        (lambda (newlat seen) (col newlat (cons (car lat) seen)))))
    (else (multiremeber a (cdr lat) 
        (lambda (newlat seen) (col (cons (car lat) newlat) seen))))))

(define a-friend (lambda (x y) (length y)))

(p (multiremeber 'nice lat a-friend))


;; old style of insertLR
;; insert on the left and the right
(define (mulLR new oldL oldR lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) oldL) (append (list new oldL) (mulLR new oldL oldR (cdr lat))))
    ((eq? (car lat) oldR) (append (list oldR new) (mulLR new oldL oldR (cdr lat))))
    (else (cons (car lat) (mulLR new oldL oldR (cdr lat))))))

(p (mulLR 'ins 'is 'nice lat))


;; refactor using collector 









