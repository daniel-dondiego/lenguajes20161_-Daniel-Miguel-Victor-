#lang plai

(require "practica4-base.rkt")

(print-only-errors true) 

;1.desugar:Dada una expresión en FAES la devuelve en FAE
(define (desugar expr)
  (type-case FAES expr
    [numS (n) (num n)]
    [withS (bindings b) (app (fun (map (lambda (x)(bind-name x)) bindings)(desugar b))(map (lambda (bind)(desugar (bind-val bind))) bindings))]
    ;3.with*: Adecua desugar para obtener para recibir expresiones with*
    [with*S (bindings b) (app (fun (map (lambda (x) (bind-name x)) bindings) (desugar b))(map (lambda (x) (desugar (bind-val x))) bindings))]
    [idS (e) (id e)]
    [funS (params b) (fun params (desugar b))]
    [appS (f lst) (app (desugar f) (map desugar lst))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]))

(define (cparse sexp)
  (desugar (parse sexp)))

;;lookup para interp
(define (lookup name env)
  (cond
    [(mtSub? env)]
    [(aSub? env) (if (equal? (aSub-name env) name) (aSub-value env) (lookup name (aSub-env env)))]))

;4.interp:
;; evalua una expresion dada en la gramática FAE
(define (interp expr env)
 (type-case FAE expr
   [num (n) (numV n)]
   [binop (f l r) (numV (f (numV-n (interp l env)) (numV-n (interp r env))))]
   [id (name) (lookup name env)]
   [fun (params body) (closureV params body env)]
   [app (f args) (local ([define v (interp f env)])
                   (map (lambda (x) (interp x env)) args))]))

(define (rinterp expr)
  (interp expr (mtSub)))
;----------------------------------------------------TEST----------------------------------------------------
;desugar
(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))
(test (desugar (numS 0)) (num 0))
(test (desugar (idS 'z)) (id 'z))

(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))
