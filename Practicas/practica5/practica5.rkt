#lang plai

(require "practica5-base.rkt")

(print-only-errors true)


;Desugar

(define (desugar expr)
    (type-case RCFAELS expr
        [idS (x) (id x)]
        [numS (n) (num n)]
        [boolS (b) (bool b)]
        [mListS (l) (mList l)]
        [IfS (c t e) (If (desugar c) (desugar t) (desugar e))]
        [Equal?S (x y) (Equal? (desugar x) (desugar y))]
        [opS (f o) (op f (desugar o))]
        [binopS (f l r) (binop f (desugar l) (desugar r))]
        [boolopBinS (f l r) (boolopBin f (desugar l) (desugar r))]
        [funS (params b) (fun params (desugar b))]
        [appS (fun args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]
        ;;[recS (name named-expr body) (rec name
          ;;                          (desugar named-expr)
            ;;                        (desugar body))]
        [withS (bindings b) (app (fun (map (lambda (bind) 
                                        (bind-name bind)) bindings)
                                        (desugar b))
                                    (map (lambda (bind)
                                        (desugar (bind-val bind))) bindings))]
        [with*S (bindings b) (matryoshka bindings b)]))

(define (matryoshka l b)
    (cond
        [(empty? l) (desugar b)]
        [else (app (fun (list (name-l (car l)))
                        (matryoshka (cdr l) b))
                        (list (desugar (val-l (car l)))))]))


;Interprete
(define (interp expr env)
    (type-case FCFAEL expr
        [num (n) (numV n)]
        [id (n) (lookup n env)]
        [bool (b) (boolV b)]
        [mList (l) (mListV l)]
        [If (c t e) (if (evalBool (interp c env))
                        (interp t env)
                        (interp e env))]
        [Equal? (x y) (evalEqual (interp x env) (interp y env))]
        [op (f l) (opV f (interp l env))]
        [binop (f l r) (opV f (interp l env) (interp r env))]
        [boolopBin (f l r) (boolopV f (interp l env) (interp r env))]
        ;;[rec (bound-id named-expr bound-body)
          ;;  (interp bound-body
            ;;(ciclo bound-id
                    named-expr
                    env))]
        [fun (params f) (closureV params f env)]
        [app (fun-expr arg-expr) 
             (local ([define fun-val (interp fun-expr env)])
               (if (checkAll arg-expr env)
                   (interp (closureV-body fun-val)
                           (aux (closureV-param fun-val) arg-expr (closureV-env fun-val)))
                   (error "Un simbolo no esta en el ambiente")))]))


(define (evalBool c)
    (type-case FCAEL-Value c
        (cond
            [boolV (b) b]
            [else (error "No es de tipo bool")]


(define (evalEqual x y)
  (cond
    [(and (numV? x) (numV? y)) (boolV (Equal? (numV-n x) (numV-n y)))]
    [(and (boolV? x) (boolV? y)) (boolV (Equal? (boolV-b x) (boolV-b y)))]
    [(and (mListV? x) (mListV? y)) (boolV (listasIgual? (lambda (w z) (Equal? w z)) (mListV-l x) (MListV-l y)))] ;falta implementar listasIgual
    [else (error "Incorrecto")]))
