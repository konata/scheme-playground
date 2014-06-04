(define (p . w)
  (display w)
  (newline))


(define (void param)
  (void param))


(define list-0 '())
(define list-1 '(1))
(define list-2 '(1 2))
(define list-3 '(1 2 3))


;; length0
(lambda (length)
  (lambda (l)
    (cond 
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))

(define (add1 n)
  (+ 1 n))

;; length-n
;(p
(
 (
  (lambda (mk-length)
  (mk-length mk-length))

  (lambda (mk-length)
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (mk-length (cdr l)))))))
  )

list-3)
;)






