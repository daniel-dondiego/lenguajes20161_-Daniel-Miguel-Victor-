#lang plai

(require "practica5-base.rkt")

(print-only-errors true)


;Desugar
(define (desugar expr)
  (type-case RCFAELS expr
     [ifS (i j k) 
           (If (desugar i)
                (desugar j)
                (desugar k))]
    [recS (id expr body) (rec id (desugar expr) (desugar body))]
    [equal?S (id1 id2) (Equals? (desugar id1)(desugar id2))]
    [mListS (i j) (Mlist (desugar i)(desugar j))]
    [numS (n) (num n)]
    [boolS (v) (bool v)]
    [idS (s) (id s)]
    [funS (params body) (fun params (desugar body))]
    [appS (fun args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]
    [opS (f l)(op f(desugar l))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [withS (bindings body)                   
	   (app (fun (map (lambda (bind)
			    (bind-name bind)) bindings) 
		     (desugar body))                             
		(map (lambda (bind)            
	       (desugar (bind-val bind))) bindings))]
    [with*S (bindings body)
             (matryoshka bindings body)]))

(define (matryoshka b body)
  (cond
    [(empty? b) (desugar body)]
    [else (app (fun (list (bind-name (car b)))
                    (matryoshka (cdr b) body))
                    (list (desugar (bind-val (car b)))))] ))

;Interp
(define (interp expr env)
  (type-case RCFAEL expr
     [If (c t e)
         (if (bool (interp c env))
             (interp t env)
             (interp e env))]
    [Equals? (id1 id2)(equalsV (interp id1 env)(interp id2 env))]
    [rec (id expr body)
      (interp body
              (cyclically-bind-and-interp id
                                          expr
                                          env))]
    [with  (bound-id named-expr bound-body)
           (interp bound-body
                   (aSub bound-id
                         (interp named-expr env) env))] 
    [Mlist(e lst) (Cons (interp e env) (interp lst env))]
    [bool (v) (boolV v)]
    [id (n) (lookup n env)]
    [num (n) (numV n)]
    [op (f l)(opU f(interp l env))]
    [binop (f l r) (opB f (interp l env) (interp r env))]
    [fun (fun-id fun-body)
         (closureV fun-id fun-body env)]
    [app (fun-id fun-body)
         (local([define fun-val(interp fun-id env)])
               (interp (closureV-body fun-val)
                      (args (closureV-param fun-val)
                            (map (lambda (arg) (interp arg env)) fun-body)
                            (closureV-env fun-val))))]))

(define (cyclically-bind-and-interp bound-id named-expr e)
  (local ([define value-holder (box (numV 1729))]
          [define new-env (aRecSub bound-id value-holder e)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))

;auxiliar para equals
(define (equalsV id1 id2)
  (cond
    [(and (numV? id1) (numV id2))(boolV (= id1 id2))]
    [(and (boolV? id1)(boolV? id2)) (boolV (equal? id1 id2))]
    [(and (list? id1) (list? id2))(boolV (= (length id1) (length id2)))]
    [else "La aplicación de equal? no es adecuada"]))

;mete argumentos al ambiente
(define (args param arg e)
  (cond
    [(empty? param) e]
    [else (aSub (car param) (car arg) (args (cdr param) (cdr arg) e))]))

;unarias
(define (opU f p)
  (cond
   [(numV? p) (let ((res (f (numV-n p))))
                   (if (number? res) (numV res) (boolV res)))]
   [(boolV? p) (let ((res (f (boolV-b p))))
                   (if (number? res) (numV res)(boolV res)))]))

;binarias
(define (opB f p1 p2)
   (cond
      [(and (numV? p1) (numV? p1)) (let ((res (f (numV-n p1) (numV-n p2))))
                                     (if (number? res) (numV res) (boolV res)))]
      [(and (boolV? p1) (boolV? p2))(let ((res (f (boolV-b p1) (boolV-b p2))))
                                     (if (boolean? res) (boolV res)(error "Insuficientes")))]))

;lookup
(define (lookup n e)
  (type-case Env e
    [mtSub () (error 'lookup "no hay identificador")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name n)bound-value
              (lookup n rest-env))]
    [aRecSub (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name n)
                 (unbox boxed-bound-value)
                 (lookup n rest-env))] ))

(define (rinterp expr)
  (interp expr (mtSub)))

(define (cparse sexp)
  (desugar (parse sexp)))