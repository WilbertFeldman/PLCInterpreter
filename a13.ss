					; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    (eval-exp form (empty-env))))

(define eval-exp
  (lambda (exp env)
    (cases expression exp
	   [lit-exp (datum)
		    (if (pair? datum)
			(cadr datum)
			datum)]
	   [var-exp (id)
		    (apply-env env id init-env)]
	   [app-exp (rator rands)
		    (let ([proc-value (eval-exp rator env)]
			  [args (eval-rands rands env)])
		      (apply-proc proc-value args env))]
	   [if-exp (condition body1 body2)
		   (if (eval-exp condition env)
		       (eval-exp body1 env)
		       (eval-exp body2 env))]
	   [let-exp (vars vals bodies)
		    (let ([new-env (extend-env vars (map (lambda (x) (eval-exp x env)) vals) env)])
		      (eval-bodies bodies new-env))]
	   [lambda-exp (vars bodies)
		       (closure vars bodies env)]
	   [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define eval-bodies
  (lambda (lst env)
    (cond
     [(null? (cdr lst)) (eval-exp (car lst) env)]
     [else (eval-exp (car lst) env) (eval-bodies (cdr lst) env)])))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

(define apply-proc
  (lambda (proc-value args env)
    (cases proc-val proc-value
	   [prim-proc (op) (apply-prim-proc op args)]
	   [closure (vars bodies env)
		    (let ([new-env (add-lambda-variables-to-enviornment vars args env)])
		      (eval-bodies bodies new-env))]
	   [else (error 'apply-proc
			"Attempt to apply bad procedure: ~s"
			proc-value)])))

;; I want a better name for this
(define (add-lambda-variables-to-enviornment vars args env)
  (cond [(symbol? vars)
	 (extend-env (list vars) (list args) env)]
	[(list? vars)
	 (extend-env vars args env)]
	[else
	 (display "I don't feel like handling this case rn")
	 ]))


(define *prim-proc-names* '(+ - * / add1 sub1 = < > <= >= not zero? cons car cdr cddr
			      list null? assq eq? equal? atom? length list->vector
			      list? pair? procedure? vector->list vector make-vector
			      vector-ref vector? number? symbol? set-car! set-cdr!
			      vector-set! display newline caar cadr cdar cddr caaar
			      caadr cadar cdaar caddr cdadr cddar cdddr))

(define init-env
  (extend-env
   *prim-proc-names*
   (map prim-proc
	*prim-proc-names*)
   (empty-env)))

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (apply add1 args)]
      [(sub1) (apply sub1 args)]
      [(=) (apply = args)]
      [(<) (apply < args)]
      [(>) (apply > args)]
      [(<=) (apply <= args)]
      [(>=) (apply >= args)]
      [(not) (apply not args)]
      [(zero?) (apply zero? args)]
      [(cons) (apply cons args)]
      [(car) (apply car args)]
      [(cdr) (apply cdr args)]
      [(list) args]
      [(null?) (apply null? args)]
      [(assq) (apply assq args)]
      [(eq?) (apply eq? args)]
      [(equal?) (apply equal? args)]
      [(atom?) (apply atom? args)]
      [(length) (apply length args)]
      [(list->vector) (apply list->vector args)]
      [(list?) (apply list? args)]
      [(pair?) (apply pair? args)]
      [(procedure?) (apply procedure? args)]
      [(vector->list) (apply vector->list args)]
      [(vector) (apply vector args)]
      [(make-vector) (apply make-vector args)]
      [(vector-ref) (apply vector-ref args)]
      [(vector?) (apply vector? args)]
      [(number?) (apply number? args)]
      [(symbol?) (apply symbol? args)]
      [(set-car!) (apply set-car! args)]
      [(set-cdr!) (apply set-cdr! args)]
      [(vector-set!) (apply vector-set! args)]
      [(display) (apply display args)]
      [(newline) (apply newline args)]
      [(caar) (apply caar args)]
      [(cadr) (apply cadr args)]
      [(cdar) (apply cdar args)]
      [(cddr) (apply cddr args)]
      [(caaar) (apply caaar args)]
      [(caadr) (apply caadr args)]
      [(cadar) (apply cadar args)]
      [(cdaar) (apply cdaar args)]
      [(caddr) (apply caddr args)]
      [(cdadr) (apply cdadr args)]
      [(cddar) (apply cddar args)]
      [(cdddr) (apply cdddr args)]
      [else (error 'apply-prim-proc
		   "Bad primitive procedure name: ~s"
		   prim-op)])))

(define rep
  (lambda ()
    (display "--> ")
    (let ([answer (top-level-eval (parse-exp (read)))])
      (eopl:pretty-print answer) (newline)
      (rep))))

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))
;; Parsed expression datatype.  You will probably want to replace this
;; with your expression datatype from A11b.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (value literal?)]
  [lambda-exp
   (vars (lambda (x) (or ((list-of symbol?) x) (improper-list-of-symbols? x) (symbol? x))))
   (bodies (list-of expression?))]
  [if-exp
   (condition expression?)
   (body1 expression?)
   (body2 expression?)]
  [let-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [let*-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [letrec-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [namedlet-exp
   (name var-exp?)
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [set!-exp
   (var var-exp?)
   (body expression?)]
  [app-exp
   (rator expression?)
   (rands (list-of expression?))])


					;type helpers
(define var-exp?
  (lambda (x)
    (if (expression? x)
	(cases expression x
	       [var-exp (id) #t]
	       [else #f])
	#f)))

(define (improper-list-of-symbols? list)
  (if (pair? list)
      (and (symbol? (1st list)) (improper-list-of-symbols? (cdr list)))
      (symbol? list)))

(define (literal? x)
  (and (not (symbol? x)) (or (not (pair? x)) (equal? 'quote (car x)))))

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

					; datatype for procedures.  At first there is only one
					; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (args (lambda (x) (or ((list-of symbol?) x) (improper-list-of-symbols? x) (symbol? x))))
   (bodies (list-of expression?))
   (env environment?)])
; Environment definitions for CSSE 304 Scheme interpreter.
					; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
	    [(eq? sym (car los)) pos]
	    [else (loop (cdr los) (add1 pos))]))))

(define apply-global-env
  (lambda (env sym)
    (cases environment env
	   [extended-env-record (syms vals env)
				(let ([pos (list-find-position sym syms)])
				  (if (number? pos)
				      (list-ref vals pos)
				      (eopl:error 'env "variable ~s not found." sym)))]
	   [else (eopl:error 'env "Not extended-env-record")])))


(define apply-env
  (lambda (env sym global-env)
    (cases environment env
	   [empty-env-record ()
			     (apply-global-env global-env sym)]
	   [extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
				  (if (number? pos)
				      (list-ref vals pos)
				      (apply-env env sym global-env)))])))
					; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

					; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

					; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp
  (lambda (datum)
    (cond
     [(symbol? datum)
      (var-exp datum)]
     [(literal? datum)
      (lit-exp datum)]
     [(list? datum)
      (cond
       [(eqv? (1st datum) 'lambda)
	(parse-lambda datum)]
       [(equal? (1st datum) 'if)
	(parse-if datum)]
       [(and (equal? (1st datum) 'let)
	     (and (not (null? (cdr datum))) (symbol? (2nd datum))))
	(parse-named-let datum)]
       [(equal? (1st datum) 'let)
	(parse-let let-exp datum)]
       [(equal? (1st datum) 'let*)
	(parse-let let*-exp datum)]
       [(equal? (1st datum) 'letrec)
	(parse-let letrec-exp datum)]
       [(equal? (1st datum) 'set!)
	(parse-set! datum)]
       [else
	(parse-app datum)])]
     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (valid-vars vars)
  (cond [(list? vars)
	 (andmap symbol? vars)]
	[(pair? vars)
	 (improper-list-of-symbols? vars)]
	[else
	 (symbol? vars)]))
					;4 helpers
(define (parse-lambda datum)
  (cond [(not (valid-vars (2nd datum)))
	 (eopl:error 'parse-exp "invalid variables for lambda: ~s" datum)]
	[(> 3 (length datum))
	 (eopl:error 'parse-exp "improperly formated lambda expected >2 parts: ~s" datum)]
	[else
	 (cond
	  [(symbol? (2nd datum))
	   (lambda-exp (2nd datum)
		       (map parse-exp (cddr datum)))]
	  [(not (list? (2nd datum)))
	   (lambda-exp (2nd datum)
		       (map parse-exp (cddr datum)))]
	  [else
	   (lambda-exp (2nd datum)
		       (map parse-exp (cddr datum)))])]))

(define (parse-if datum)
  (cond
   [(not (= (length datum) 4))
    (eopl:error 'parse-exp "if requires 4 parts: cond, body1, body2. received: ~s" datum)]
   [else
    (if-exp (parse-exp (2nd datum))
	    (parse-exp (3rd datum))
	    (parse-exp (cadddr datum)))]))

(define (parse-let type datum)
  (cond
   [(> 3 (length datum))
    (eopl:error 'parse-exp "length is less than 3")]
   [(not (and
	  (list? (2nd datum))
	  (andmap (lambda (x)
		    (and (list? x) (= 2 (length x)) (symbol? (1st x))))
		  (2nd datum))))
    (eopl:error 'parse-exp "first elements must be symbols")]
   [else
    (type
     (map 1st (2nd datum))
     (map (lambda (x) (parse-exp (2nd x))) (2nd datum))
     (map parse-exp (cddr datum)))]))

(define (parse-named-let datum)
  (cond
   [(> 3 (length datum)) (eopl:error 'parse-exp "length is less than 4")]
   [(and (list? (2nd datum))
	 (andmap (lambda (x)
		   (and (list? x) (= 2 (length x)) (symbol? (1st x))))
		 (2nd datum)))
    (eopl:error 'parse-exp "first elements must be symbols")]
   [(symbol? (2nd datum)) (eopl:error 'parse-exp "let must be named")]
   [else
    (namedlet-exp (2nd datum)
		  (map (lambda (x) (parse-exp (1st x))) (3rd datum))
		  (map (lambda (x) (parse-exp (2nd x))) (3rd datum))
		  (map parse-exp (cadddr datum)))]))

(define (parse-set! datum)
  (cond
   [(not (= (length datum) 3))
    (eopl:error 'parse-exp "length is not equal to 3")]
   [(not (symbol? (2nd datum)))
    (eopl:error 'parse-exp "the first element must be a symbol")]
   [else
    (set!-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]))

(define (parse-app datum)
  (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum))))

(define (improper-list-map proc list)
  (if (pair? x)
      (cons (proc (car list)) (improper-list-map proc (cdr list)))
      (proc x)))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
	   [var-exp (exp) exp]
	   [lit-exp (exp) exp]
	   [lambda-exp (vars bodies)
		       (cond
			[(symbol? vars)
			 (append (list 'lambda vars)
				 (map unparse-exp bodies))]
			[(list? vars)
			 (append (list 'lambda vars)
				 (map unparse-exp bodies))]
			[(pair? vars)
			 (append (list 'lambda vars)
				 (map unparse-exp bodies))])]
	   [if-exp (conds body1 body2)
		   (list 'if
			 (unparse-exp conds)
			 (unparse-exp body1)
			 (unparse-exp body2))]
	   [let-exp (vars vals bodies)
		    (append (list 'let
				  (map (lambda (x y) (list x (unparse-exp y))) vars vals))
			    (map unparse-exp bodies))]
	   [let*-exp (vars vals bodies)
		     (append (list 'let*
				   (map (lambda (x y) (list x (unparse-exp y))) vars vals))
			     (map unparse-exp bodies))]
	   [letrec-exp (vars vals bodies)
		       (append (list 'letrec
				     (map (lambda (x y) (list x (unparse-exp y))) vars vals))
			       (map unparse-exp bodies))]
	   [namedlet-exp (name vars vals bodies)
			 (list 'let
			       (unparse-exp name)
				     (map (lambda (x y) (list x (unparse-exp y))) vars vals))
			       (map unparse-exp bodies)]
	   [set!-exp (var exp)
		     (list 'set! (unparse-exp var) (unparse-exp exp))]
	   [app-exp (rator rands)
		    (cons (unparse-exp rator) (map unparse-exp rands))])))

    (define (improper-list-map proc list)
      (if (pair? x)
	  (cons (proc (car list)) (improper-list-map proc (cdr list)))
	  (proc x)))
