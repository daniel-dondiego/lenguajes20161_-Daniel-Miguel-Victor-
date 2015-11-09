#lang plai

(define-type Binding
  [bind (name symbol?) (val RCFAELS?)])
  
(define-type MListFAELS
  (MEmptyS)
  (MConsS [mcar RCFAELS?] [mcdr MListFAELS?]))
  
(define-type MListS
  (MEmpty)
  (MCons [mcar RCFAEL?] [mcdr MListS?]))
  
(define-type MListV
  (MEmptyV)
  (MConsV [mcar RCFAEL-Value?] [mcdr MListV?]))

(define-type RCFAELS
  [numS (n number?)]
  [boolS (v boolean?)]
  [idS (name symbol?)]
  [funS (params (listof symbol?))
       (body RCFAELS?)]
  [appS (f RCFAELS?)
       (args (listof RCFAELS?))]
  [opS (f procedure?)
      (args RCFAELS?)]
  [binopS (f procedure?)
         (l RCFAELS?)
         (r RCFAELS?)]
  [withS (bindings (listof bind?))(body RCFAELS?)]
  [with*S (bindings 
         (listof bind?))(body RCFAELS?)]
  [ifS (c RCFAELS?)
       (t RCFAELS?)
       (e RCFAELS?)]
  [recS (id RCFAELS?) (expr RCFAELS?) (body RCFAELS?)]
  [equal?S (id1 RCFAELS?)(id2 RCFAELS?)]
  [mListS (e RCFAELS?)(lst RCFAELS?)] )


;RCFAEL type definition
(define-type RCFAEL
  [id (name symbol?)]
  [num (n number?)]
  [bool (v boolean?)]
  [Mlist (e RCFAEL?) (lst RCFAEL?)]
  [with (name symbol?) (named-expr RCFAEL?) (body RCFAEL?)]
  [rec (id RCFAEL?) (expr RCFAEL?) (body RCFAEL?)]
  [fun (params (listof symbol?))
       (body RCFAEL?)]
  [If (cond RCFAEL?)
        (then RCFAEL?)
        (else RCFAEL?)]
  [Equals? (id1 RCFAEL?)
          (id2 RCFAEL?)]
  [app (fun RCFAEL?)
       (args (listof RCFAEL?))]
  [binop (f procedure?)
         (l RCFAEL?)
         (r RCFAEL?)]
  [op (f procedure?)
      (args RCFAEL?)])


;Type-Value
(define-type RCFAEL-Value
  [numV (n number?)]
  [closureV (param (listof symbol?))
	    (body RCFAEL?)
	    (env Env?)]
  [boolV (b boolean?)]
  [mlistV (ml MList?)])

(define-type MList
  [MEmpty]
  [Cons (e RCFAEL-Value?) (l MList?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value RCFAEL-Value?)
        (env Env?)]
  [aRecSub (name symbol?)
           (value boxed-RCFAEL-Value?)
           (env Env?)])

;box
(define (boxed-RCFAEL-Value? v)
  (and (box? v)
       (RCFAEL-Value? (unbox v))))


(define (parse-bindings lst allow)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (or (boolean? bindRep) allow)
	(map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
	(error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))

(define (elige s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(<) <]
    [(<=) <=]
    [(>) >]
    [(>=) >=]
    [(and) (lambda (x y) (and x y))]
    [(or) (lambda (x y) (or x y))]
    [(inc) add1]
    [(dec) sub1]
    [(zero?) zero?]
    [(num?) num?]
    [(neg) not]
    [(bool?) boolean?]
    [(first) first]
    [(rest) rest]
    [(empty?) empty?]
    [(list?) list?]))



(define (buscaRepetido l comp)
  (cond
   [(empty? l) #f]
   [(member? (car l) (cdr l) comp) (car l)]
   [else (buscaRepetido (cdr l) comp)]))


(define (member? x l comparador)
  (cond
   [(empty? l) #f]
   [(comparador (car l) x) #t]
   [else (member? x (cdr l) comparador)]))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> RCFAELS
(define (parse sexp)
  (cond
   [(number? sexp) (numS sexp)]
   [(boolean? sexp) (boolS sexp)]
   [(symbol? sexp) (idS sexp)]
   [(list? sexp)
    (case (car sexp)
      [(equal?) (Equals? (parse (cadr sexp)) (parse (caddr sexp)))]
      [(if)(ifS (parse(cadr sexp)) (parse(caddr sexp))(parse(cadddr sexp)))]
      [(with) (withS (parse-bindings (cadr sexp) #f) (parse (caddr sexp)))]
      [(with*) (with*S (parse-bindings (cadr sexp) #t) (parse (caddr sexp)))]
      [(fun) (funS (cadr sexp) (parse (caddr sexp)))]
      [(+ - / * < > <= >= and or) (binopS (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
      [(inc dec zero? num? neg bool? first rest empty? list?) (opS (elige (car sexp)) (parse (cadr sexp)))]
      [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))
