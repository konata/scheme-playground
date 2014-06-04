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

(define (mulLR new oldL oldR lat col)
  (cond
    ((null? lat) (col '() 0 0))
    (else (let ((header (car lat)) (tail (cdr lat)))
                (cond 
                  ((eq? header oldL) (mulLR new oldL oldR (cdr lat) 
                      (lambda (newlat L R) (col (cons new (cons oldL newlat)) (+ 1 L) R)
                      )))
                  ((eq? header oldR) (mulLR new oldL oldR (cdr lat) 
                      (lambda (newlat L R) (col (cons oldR (cons new newlat)) L (+ 1 R))
                      )))
                  (else (mulLR new oldL oldR tail 
                      (lambda (newlat L R) (col (cons header newlat) L R))
                      )))))))

(define (col lat L R) (p lat) (p L) (p R))

(mulLR 'new 'is 'nice lat col)

;; begin even? without collector
(define (even lat)
  (cond
    ((null? lat) '())
    (else (let ((header (car lat)) (tail (cdr lat)))
                (cond
                  ((and (atom? header) (even? header)) (cons header (even tail)))
                  ((atom? header) (even tail))
                  (else (cons (even header)  (even tail))))))))
  
(define even-list '(1 2 3 (2 3 4 (7 8 9 4)) 5 6 7 8 9 10))
(p (even even-list))
  


  

