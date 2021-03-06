;; helper for print with newline
(define p 
  (lambda (x)  
    (newline)
    (display x)))


;; atom is not pair and not null
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


;; every element in lat is atom
(define lat?
  (lambda (x) 
    (cond
      ((null? x) #t)
      ((atom? (car x))  (lat? (cdr x)))
      (else #f))))


;; ele is a member of lat
(define member? 
  (lambda (ele lat)
    (cond 
      ((null? lat) #f)
      (else (or (eq? (car lat) ele) (member? ele (cdr lat)))))))


;; remove member ele from lat
(define remeber
  (lambda (ele lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond 
         ((eq? ele (car lat)) (cdr lat))
         (else (cons (car lat) (remeber ele (cdr lat)))))))))


;; better-version of rember
(define remeber-revised
  (lambda (ele col)
    (cond 
      ((null? col) '())
      ((eq? ele (car col)) (cdr col))
      (else (cons (car col) (remeber-revised ele (cdr col)))))))


;; the firsts items in each list
(define firsts
  (lambda (col)
    (cond 
      ((null? col) '())
      (else (cons (car (car col)) (firsts (cdr col))))
      )))


;; insert new items right to the old inthe lat
(define insert
  (lambda (new old lat)
  (cond 
    ((null? lat) '())
    ((eq? old (car lat)) (cons old (cons new (cdr lat))))
    (else (cons (car lat) (insert new old (cdr lat)))
  ))))


;; replace in range
(define subst
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) old1) (eq? (car lat) old2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old1 old2 (cdr lat)))))))


;; multirember 
(define multirember
  (lambda (ele lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) ele) (multirember ele (cdr lat)))
      (else (cons (car lat) (multirember ele (cdr lat)))))))


;; multiinsert-r
(define multiinsert-r
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (multiinsert-r new old (cdr lat)))))
      (else (cons (car lat) (multiinsert-r new old (cdr lat)))))))



;; begin main
(p (lat? '(this is an nice 'item)))
(p (lat? '(this is an nice item)))
(p (member? 'x '(a b c)))
(p (remeber 'x '(x y z)))
(p (remeber 'y '(x y z)))
(p (remeber 'z '(x y z)))
(p (remeber-revised 'c '(a b c d e f g)))
(p (firsts '((this is an item) (that is also an item) (and me is also an item))))
(p (subst 'new 'old1 'old2 '(new this is an nice old1 and that is also old2)))
(p (insert 'new 'old '(this is an nice old item and you should recv)))
(p (insert 'new 'old '(this is an nice item and you should recv)))
(p (multirember 'nice '(this is an nice this is an nice nice nice)))
(p (multiinsert-r 'new 'old '(this is an old and that is also an old and old)))













