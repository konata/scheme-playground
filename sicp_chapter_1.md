### p40


> The formal parameters of a procedure are local to the body
> of the procedure

===

> A formal parameter of a procedure has a very special role in the procedure definition, in that it doesn't matter what name the formal parameter has. Such a name is called a bound variable,and we say that the procedure definition binds its formal parameters. 

===
> we allow a procedure to have internal definitions that are local to that procedure,Such nesting of definitions, called block structure,


```
(define (sqrt x)  (define (good-enough? guess x)    (< (abs (- (square guess) x)) 0.001))  (define (improve guess x)    (average guess (/ x guess)))  (define (sqrt-iter guess x)    (if (good-enough? guess x)        guess        (sqrt-iter (improve guess x) x)))  (sqrt-iter 1.0 x))
  ```
  
  > as shown below. Then x gets its value from the argument with which the enclosing procedure sqrt is called. This discipline is called lexical scoping
  
===  >Lexical scoping dictates that free variables in a procedure are taken to refer to bindings made by enclosing procedure definitions; that is, they are looked up in the environment in which the procedure was defined. 
 
 1.  Linear Recursion and Iteration
 2.  Tree Recursion
 
 
 
